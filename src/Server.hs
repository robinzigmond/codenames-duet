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
                                                 ToJSON (..), Value (Object),
                                                 decode, defaultOptions, encode,
                                                 genericParseJSON,
                                                 genericToJSON, sumEncoding,
                                                 tagSingleConstructors)
import           Data.ByteString.Lazy           (ByteString)
import           GHC.Generics                   (Generic)
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Application.Static (defaultWebAppSettings,
                                                 ssIndices, ssLookupFile,
                                                 staticApp)
import           Network.WebSockets             (Connection, ServerApp,
                                                 acceptRequest, forkPingThread,
                                                 receiveData, sendTextData)
import           Text.Pandoc.UTF8               (fromStringLazy, fromTextLazy,
                                                 toTextLazy)
import           WaiAppStatic.Types             (unsafeToPiece)

import           GamePlay                       (Card, randomCardsIO)

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

type GameId = ByteString

data Game =
  Game
    { gameId  :: GameId
    , player1 :: Maybe ClientId
    , player2 :: Maybe ClientId
    , cards   :: [[Card]]
    }

data State =
  State [Client] [Game]

data MessageIn =
  JoinedGame GameId
  deriving (Eq, Generic)

customOptions :: Options
customOptions =
  defaultOptions
    {sumEncoding = TaggedObject "type" "message", tagSingleConstructors = True}

instance FromJSON ByteString where
  parseJSON = fmap fromTextLazy . parseJSON

instance ToJSON ByteString where
  toJSON = toJSON . toTextLazy

instance FromJSON MessageIn where
  parseJSON = genericParseJSON customOptions

data MessageOut =
  CardsForGame [[Card]]
  deriving (Eq, Generic)

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

listen :: Connection -> ClientId -> MVar State -> IO ()
listen conn clientId stateRef =
  forever $ do
    message <- receiveData conn
    msgToSend <- respond clientId stateRef message
    -- send data back
    sendTextData conn $ encode msgToSend

respond :: ClientId -> MVar State -> ByteString -> IO Value
respond clientId stateRef msg = do
  State clients _ <- readMVar stateRef
  case (decode msg) of
    Nothing -> return "" -- TODO: send back some sort of error response
    Just decoded ->
      case decoded of
        JoinedGame id
        -- generate new game with a random set of cards, and assign the new
        -- client to that game
         -> do
          cards <- randomCardsIO
          -- add cards and player to state in MVar. Much work still needed!
          modifyMVar_ stateRef $ \state@(State clients games) ->
            case filter ((== id) . gameId) games of
              (game:_) ->
                case joinGame clientId game -- TODO: move this check "further out"!
                      of
                  Just joinedGame -> do
                    let withCards = joinedGame {cards = cards}
                    return $ State clients (withCards : games)
                  Nothing -> return state
              [] -> return state
          return . toJSON $ CardsForGame cards

wsApp :: MVar State -> ServerApp
wsApp stateRef pendingConn = do
  conn <- acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  forkPingThread conn 30
  finally (listen conn clientId stateRef) (disconnectClient clientId stateRef)
