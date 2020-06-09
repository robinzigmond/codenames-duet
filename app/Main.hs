module Main where

import           Control.Concurrent             (newMVar)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (defaultConnectionOptions)

import           Lib                            (State (..), httpApp, wsApp)

main :: IO ()
main = do
  state <- newMVar (State [] [])
  run 3000 $ websocketsOr defaultConnectionOptions (wsApp state) httpApp
