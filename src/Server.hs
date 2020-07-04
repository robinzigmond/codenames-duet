{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( wsApp
  , httpApp
  , State(..)
  ) where

import           Control.Concurrent             (MVar, modifyMVar, modifyMVar_,
                                                 readMVar)
import           Control.Exception              (finally)
import           Control.Monad                  (forM_, forever)
import           Data.Aeson                     (FromJSON (..), Options,
                                                 SumEncoding (TaggedObject),
                                                 ToJSON (..),
                                                 Value (Number, Object, String),
                                                 decode, defaultOptions, encode,
                                                 genericParseJSON,
                                                 genericToJSON, sumEncoding,
                                                 tagSingleConstructors)
import           Data.ByteString.Lazy           (ByteString)
import           Data.Char                      (isDigit)
import           Data.Maybe                     (listToMaybe)
import qualified Data.Text                      as T (pack, span, unpack)
import           GHC.Generics                   (Generic)
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Application.Static (defaultWebAppSettings,
                                                 ssIndices, ssLookupFile,
                                                 staticApp)
import           Network.WebSockets             (Connection, ServerApp,
                                                 acceptRequest, forkPingThread,
                                                 receiveData, sendTextData)
import           System.Random                  (getStdGen, randomRs)
import           Text.Pandoc.UTF8               (fromStringLazy, fromTextLazy,
                                                 toStringLazy, toTextLazy)
import           WaiAppStatic.Types             (unsafeToPiece)

import           GamePlay                       (randomCardsIO, randomKeyCardIO,
                                                 takeUniques)
import           GameTypes

httpApp :: Application
httpApp =
  staticApp $
  (defaultWebAppSettings publicRoot)
    { ssIndices = [indexPiece]
    , ssLookupFile =
        \pieces ->
          case pieces of
            [_] -> defaultLookup [indexPiece]
            _   -> defaultLookup pieces
    }
  where
    indexPiece = unsafeToPiece "index.html"
    defaultLookup = ssLookupFile $ defaultWebAppSettings publicRoot
    publicRoot = "react/codenames-duet-frontend/build"

-- server code initially taken from
-- https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm
-- and subsequently adapted to my own needs
type ClientId = Int

type Client = (ClientId, Connection)

data GameId =
  GameId
    { num  :: Integer
    , rand :: ByteString
    }
  deriving (Eq, Show)

data CurrentPlayer
  = Player1
  | Player2
  | Neither

data Game =
  Game
    { gameId        :: GameId
    , player1       :: Maybe ClientId
    , player2       :: Maybe ClientId
    , cards         :: [[Card]]
    , keyCard       :: KeyCard
    , currentPlayer :: CurrentPlayer
    }

data State =
  State [Client] [Game]

data MessageIn
  = NewGame
  | JoinedGame GameId
  | ClueGiven ByteString Int
  | CardGuessed Int Int
  deriving (Eq, Show, Generic)

customOptions :: Options
customOptions =
  defaultOptions
    {sumEncoding = TaggedObject "type" "message", tagSingleConstructors = True}

instance FromJSON ByteString where
  parseJSON = fmap fromTextLazy . parseJSON

instance ToJSON ByteString where
  toJSON = toJSON . toTextLazy

instance FromJSON GameId where
  parseJSON (String s) =
    let (numString, rand) = T.span isDigit s
     in GameId <$> parseJSON (Number . fromInteger . read $ T.unpack numString) <*>
        parseJSON (String rand)

instance FromJSON MessageIn where
  parseJSON = genericParseJSON customOptions

data MessageOut
  = Error ByteString
  | CardsForGame Int [[Card]] KeyCardSide
  | GameStarted GameId
  | CantJoin ByteString
  | ClueReceived ByteString Int
  | CardGuessedResponse Int Int CardType
  deriving (Generic)

instance ToJSON GameId where
  toJSON (GameId num rand) = String . T.pack $ show num ++ toStringLazy rand

instance ToJSON MessageOut where
  toJSON = genericToJSON customOptions

joinGame :: ClientId -> Game -> Maybe Game
joinGame clientId game
  | player1 game == Nothing = Just $ game {player1 = Just clientId}
  | otherwise =
    if player2 game == Nothing
      then Just $ game {player2 = Just clientId}
      else Nothing

nextId :: [Client] -> ClientId
nextId clients =
  case clients of
    [] -> 0
    _  -> maximum (map fst clients) + 1

connectClient :: Connection -> MVar State -> IO ClientId
connectClient conn stateRef =
  modifyMVar stateRef $ \(State clients games) -> do
    let clientId = nextId clients
    let newClient = (clientId, conn)
    return (State (newClient : clients) (games), clientId)

withoutClient :: ClientId -> [Client] -> [Client]
withoutClient clientId = filter ((/= clientId) . fst)

disconnectClient :: ClientId -> MVar State -> IO ()
disconnectClient clientId stateRef =
  modifyMVar_ stateRef $ \(State clients games) ->
    return $ State (withoutClient clientId clients) games

-- create new game (when appropriate socket message received)
-- need to generate random (unique) ID as well as random cards, and
-- add the connecting client to it as player1.
-- How to generate IDs? perhaps concatenate a number that strictly increases
-- with a randomly generated string?
newGameId :: [GameId] -> IO GameId
newGameId currentIds = do
  let integerPart =
        case currentIds of
          [] -> 1
          _  -> maximum (map num currentIds) + 1
  gen <- getStdGen
  let randomPart = fromStringLazy . takeUniques 8 $ randomRs ('a', 'z') gen
  return $ GameId integerPart randomPart

makeNewGame :: MVar State -> IO GameId
makeNewGame stateRef =
  modifyMVar stateRef $ \(State clients games) -> do
    newId <- newGameId (map gameId games)
    cards <- randomCardsIO
    keyCard <- randomKeyCardIO
    let newGame = Game newId Nothing Nothing cards keyCard Neither
    return $ (State clients (newGame : games), newId)

listen :: Connection -> ClientId -> MVar State -> IO ()
listen conn clientId stateRef =
  forever $ do
    message <- receiveData conn
    --(connectionToRespondOn, msgToSend) <- respond conn clientId stateRef message
    allToSend <- respond conn clientId stateRef message
    -- send data back
    forM_ allToSend $ \(connectionToRespondOn, msgToSend) ->
      sendTextData connectionToRespondOn $ encode msgToSend

findAs :: ClientId -> [Game] -> (Game -> Maybe ClientId) -> [Game]
findAs clientId games player = filter ((== (Just clientId)) . player) games

otherPlayer :: [Game] -> ClientId -> Maybe ClientId
otherPlayer games clientId =
  case findAs clientId games player1 of
    (g:_) -> player2 g
    [] ->
      case findAs clientId games player2 of
        (g:_) -> player1 g
        []    -> Nothing

findByClientId :: [Client] -> ClientId -> Maybe Connection
findByClientId clients clientId =
  fmap snd . listToMaybe $ filter ((== clientId) . fst) clients

otherPlayerConn :: ClientId -> [Client] -> [Game] -> Maybe Connection
otherPlayerConn clientId clients games =
  otherPlayer games clientId >>= findByClientId clients

findGameWithClient ::
     Bool -> ClientId -> [Game] -> Maybe (KeyCard -> KeyCardSide, Game)
findGameWithClient returnOther clientId allGames =
  case findAs clientId allGames player1 of
    (g:_) ->
      Just
        ( if returnOther
            then side2
            else side1
        , g)
    [] ->
      case findAs clientId allGames player2 of
        (g:_) ->
          Just
            ( if returnOther
                then side1
                else side2
            , g)
        [] -> Nothing

respond ::
     Connection
  -> ClientId
  -> MVar State
  -> ByteString
  -> IO [(Connection, Value)]
respond conn clientId stateRef msg = do
  State clients allGames <- readMVar stateRef
  case (decode msg) of
    Nothing -> return [(conn, toJSON $ Error "unrecognised message!")]
    Just decoded ->
      case decoded of
        NewGame
          -- generate new game with a random set of cards, and assign the new
          -- client to that game
         -> do
          newId <- makeNewGame stateRef
          return [(conn, toJSON $ GameStarted newId)]
        JoinedGame id
          -- add cards and player to gamestate in MVar
         -> do
          message <-
            modifyMVar stateRef $ \state@(State clients games) ->
              case filter ((== id) . gameId) games of
                (game:_) ->
                  case joinGame clientId game of
                    Just joinedGame -> do
                      let withJoined =
                            map
                              (\g ->
                                 if gameId g == id
                                   then joinedGame
                                   else g)
                              games
                      let (myPlayerNum, correctSide) =
                            case player2 joinedGame of
                              Just _  -> (2, side2)
                              Nothing -> (1, side1)
                      return $
                        ( State clients withJoined
                        , CardsForGame
                            myPlayerNum
                            (cards game)
                            (correctSide $ keyCard game))
                    Nothing ->
                      return
                        (state, CantJoin "This game already has 2 players.")
                [] -> return (state, CantJoin "This game does not exist.")
          return [(conn, toJSON message)]
        ClueGiven clueWord clueNumber ->
          case otherPlayerConn clientId clients allGames of
            Just theConn ->
              return [(theConn, toJSON $ ClueReceived clueWord clueNumber)]
            Nothing ->
              return [(conn, toJSON $ Error "no other player in this game yet")]
        CardGuessed row col ->
          case otherPlayerConn clientId clients allGames of
            Just otherConn
              -- look up game to get the keycard,
              -- and then use the connection id to find which side
             -> do
              let maybeGameAndSide = findGameWithClient True clientId allGames
              case maybeGameAndSide of
                Nothing ->
                  return
                    [ ( conn
                      , toJSON $ Error "can't find a game with this player!")
                    ]
                Just (otherSide, theGame) -> do
                  let result = lookupResult row col otherSide (keyCard theGame)
                  return
                    [ (conn, toJSON $ CardGuessedResponse row col result)
                    , (otherConn, toJSON $ CardGuessedResponse row col result)
                    ]
            Nothing ->
              return [(conn, toJSON $ Error "no other player in this game yet")]

wsApp :: MVar State -> ServerApp
wsApp stateRef pendingConn = do
  conn <- acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  forkPingThread conn 30
  finally (listen conn clientId stateRef) (disconnectClient clientId stateRef)
