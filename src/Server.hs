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
import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 Value (Object), decode, encode,
                                                 object, (.:), (.=))
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

data Game =
  Game (Maybe Client) (Maybe Client) [[Card]]

data State =
  State [Client] [Game]

data MessageType
  = JoinedGame
  | CardsForGame
  deriving (Eq, Ord, Generic)

instance FromJSON MessageType

instance ToJSON MessageType

data SocketMessage =
  SocketMessage MessageType ByteString

instance FromJSON SocketMessage where
  parseJSON (Object v) =
    SocketMessage <$> (v .: "type") <*> (fromTextLazy <$> (v .: "message"))

instance ToJSON SocketMessage where
  toJSON (SocketMessage msgtype msg) =
    object ["type" .= toJSON msgtype, "message" .= toJSON (toTextLazy msg)]

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
    -- let newGame = Game (Just newClient) Nothing cards
    return (State (newClient : clients) (games), clientId) {-newGame :-}

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
        SocketMessage JoinedGame id -- ensure joinedGame decodes to JoinedGame!
        -- generate new game with a random set of cards, and assign the new
        -- client to that game
         -> do
          cards <- randomCardsIO
          -- TODO: add cards and player to state in MVar
          -- TODO: use a "proper" JSON representation of cards (need to change
          -- SocketMessage type and instances)
          return . toJSON . SocketMessage CardsForGame . fromStringLazy $
            show cards

wsApp :: MVar State -> ServerApp
wsApp stateRef pendingConn = do
  conn <- acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  forkPingThread conn 30
  finally (listen conn clientId stateRef) (disconnectClient clientId stateRef)
