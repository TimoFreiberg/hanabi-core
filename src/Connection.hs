{-# LANGUAGE OverloadedStrings #-}

module Connection where

import Network.WebSockets
import qualified Data.ByteString.Lazy as Bytes
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.MVar
import System.IO.Unsafe(unsafePerformIO)

state :: MVar ByteString
state = unsafePerformIO (newMVar "")

clients :: MVar [(ByteString,Connection)]
clients = unsafePerformIO (newMVar [])

handleConnection :: PendingConnection -> IO ()
handleConnection pending = do
    connection <- acceptRequest pending
    putStrLn "opened connection"
    storeName connection
    loop connection
  where
    storeName connection = do
      msg <- receiveDataMessage connection
      case msg of
        Text text -> modifyMVar_ clients (return . ((text,connection):)) >> sendTextData connection ("hi " `Bytes.append` text)
        _ -> sendTextData connection ("name cant be binary" :: ByteString) >> storeName connection
    loop connection = do
      msg <- receiveDataMessage connection
      answer <- answerMsg msg
      broadcast answer
      loop connection

broadcast :: ByteString -> IO ()
broadcast txt = readMVar clients >>= mapM_ (\(_,conn) -> sendTextData conn txt)

answerMsg :: DataMessage -> IO ByteString
answerMsg (Text text) = modifyMVar_ state (return . (`Bytes.append` text)) >> readMVar state
answerMsg _ = return "send text please"
