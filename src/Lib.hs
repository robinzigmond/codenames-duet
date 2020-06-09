{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( wsApp
  , httpApp
  , State(..)
  ) where

import           Control.Concurrent             (MVar, modifyMVar, modifyMVar_,
                                                 readMVar)
import           Control.Exception              (finally)
import           Control.Monad                  (forM_, forever)
import           Data.Text                      (Text)
import           Network.HTTP.Types             (status400)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Application.Static (defaultWebAppSettings,
                                                 ssIndices, ssLookupFile,
                                                 staticApp)
import           Network.WebSockets             (Connection, ServerApp,
                                                 acceptRequest, forkPingThread,
                                                 receiveData, sendTextData)
import           WaiAppStatic.Types             (unsafeToPiece)

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
  Game (Maybe Client) (Maybe Client)

data State =
  State [Client] [Game]

nextId :: [Client] -> ClientId
nextId clients =
  case clients of
    [] -> 0
    _  -> maximum (map fst clients) + 1

connectClient :: Connection -> MVar State -> IO ClientId
connectClient conn stateRef =
  modifyMVar stateRef $ \(State clients games) -> do
    let clientId = nextId clients
    return (State ((clientId, conn) : clients) games, clientId)

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
    broadcast clientId stateRef message

broadcast :: ClientId -> MVar State -> Text -> IO ()
broadcast clientId stateRef msg = do
  State clients _ <- readMVar stateRef
  let otherClients = withoutClient clientId clients
  forM_ otherClients $ \(_, conn) -> sendTextData conn msg

wsApp :: MVar State -> ServerApp
wsApp stateRef pendingConn = do
  conn <- acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  forkPingThread conn 30
  finally (listen conn clientId stateRef) (disconnectClient clientId stateRef)
