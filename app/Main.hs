{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Connection
-- import Game
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

main :: IO ()
-- main = runServer "127.0.0.1" 8080 handleConnection
main = putStrLn "connecting..." >> WS.runClient targetIp targetPort path app

targetIp = "192.168.101.25"

targetPort = 4444

path = "/"

app conn = do
  putStrLn "Connected!"
  -- Fork a thread that writes WS data to stdout
  _ <-
    forkIO $
    forever $
    do msg <- WS.receiveData conn
       liftIO $ T.putStrLn msg
  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop
  loop
  WS.sendClose conn ("Bye!" :: Text)
