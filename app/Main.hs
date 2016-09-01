module Main where

import Connection
import GameState

import Network.WebSockets

main :: IO ()
main = runServer "127.0.0.1" 8080 handleConnection
