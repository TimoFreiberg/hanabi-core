{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Types
import Game
import Transitions

import Data.IORef
import System.IO.Unsafe

{-# NOINLINE gameRef #-}

gameRef :: IORef [Game]
gameRef = unsafePerformIO (newIORef [])

defaultGame :: IO ()
defaultGame = startGame ["1", "2", "3"]

startGame :: [PlayerId] -> IO ()
startGame [] = error "no players"
startGame (x:xs) = do
  game <- initState x xs
  writeIORef gameRef [game]
  fairPrint game

turn :: (Game -> Either GameOver Game) -> IO ()
turn f = do
  game <- getLastState
  case f game of
    Right game' -> fairPrint game' >> recordGame game'
    Left over -> print over

recordGame :: Game -> IO ()
recordGame game = modifyIORef gameRef (game :)

getLastState :: IO Game
getLastState = fmap head (readIORef gameRef)

getAllStates :: IO [Game]
getAllStates = readIORef gameRef

getStateAt :: Int -> IO Game
getStateAt i = fmap (!! i) getAllStates

printStateAt i = getStateAt i >>= prettyPrint

hint
  :: IsHint a
  => PlayerId -> a -> IO ()
hint pId h = turn (hintGiven h pId)

play :: Int -> IO ()
play i = turn (\game -> cardPlayed (cardAt i game) game)

discard :: Int -> IO ()
discard i = turn (\game -> cardDiscarded (cardAt i game) game)
