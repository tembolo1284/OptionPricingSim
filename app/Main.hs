-- app/Main.hs
module Main where

import Network.Wai.Handler.Warp (run)
import Server (app)  -- Only import app from Server
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = run 8080 app

