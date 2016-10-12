{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.List (intercalate)
import Data.String (IsString)
import Control.Lens (makeLenses)
import Data.Map (Map)

data Card = Card
  { _color :: Color
  , _number :: Number
  } deriving (Show, Eq)

data Color
  = White
  | Yellow
  | Green
  | Blue
  | Red
  deriving (Ord, Show, Eq, Bounded, Enum)

data Number
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Ord, Show, Eq, Bounded, Enum)

makeLenses ''Card

newtype PlayerId =
  PlayerId String
  deriving (Eq, Ord, Show, IsString)

data PlayerData =
  PlayerData (Map PlayerId Hand)

data Hand =
  Hand [(Card, Knowledge)]
  deriving (Show)

data Knowledge
  = None
  | Color
  | Number
  deriving (Show, Eq)

data GameState = GameState
  { _actingPlayer :: PlayerId
  , _playerData :: Map PlayerId Hand
  , _deck :: [Card]
  , _playedCards :: Map Color [Card]
  , _discardedCards :: [Card]
  , _hints :: Int
  , _fuckups :: Int
  } deriving (Show)

initGame p ps = GameState

makeLenses ''GameState

initialHints :: Int
initialHints = 7

class Pprint a  where
  pprint :: a -> String

instance Pprint Color where
  pprint = take 1 . show

instance Pprint Number where
  pprint = show . (+ 1) . fromEnum

instance Pprint Card where
  pprint (Card col num) = pprint col ++ " " ++ pprint num

instance Pprint a =>
         Pprint [a] where
  pprint xs = concat ["[", intercalate ", " (map pprint xs), "]"]
