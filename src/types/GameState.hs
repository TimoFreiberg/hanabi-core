{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import CardStack
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Types

data GameState = GameState
  { _actingPlayer :: PlayerId
  , _playerData :: Map PlayerId [Hand]
  , _deck :: CardStack
  , _playedCards :: Map Color CardStack
  , _discardedCards :: CardStack
  , _hints :: Int
  , _fuckups :: Int
  } deriving (Show)

makeLenses ''GameState

emptyState :: GameState
emptyState = GameState "" Map.empty cardStack Map.empty cardStack initialHints 0

withActingPlayer :: PlayerId -> GameState -> GameState
withActingPlayer = set actingPlayer

withPlayers ids = set playerData

initialHints :: Int
initialHints = 7
