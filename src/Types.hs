{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.List (intercalate)
import Data.String (IsString)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Card = Card
  { _color :: Color
  , _number :: Number
  } deriving (Show, Eq, Ord)

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

data GameOver =
  GameOver Int

isSucc
  :: (Bounded t, Enum t, Eq t)
  => t -> t -> Bool
num1 `isSucc` num2 = (num1, num2) `elem` zip allNums (tail allNums)
  where
    allNums = [minBound .. maxBound]

makeLenses ''Card

newtype PlayerId =
  PlayerId String
  deriving (Eq, Ord, Show, IsString)

type Hand = Map Card [Fact]

getCards :: Hand -> [Card]
getCards = Map.keys

createHand :: [Card] -> Hand
createHand cards = Map.fromList (fmap (, []) cards)

data Fact
  = ColorFact Color
  | NumberFact Number
  | Not Fact
  deriving (Show, Eq)

data GameState = GameState
  { _actingPlayer :: PlayerId
  , _playerHands :: Map PlayerId Hand
  , _deck :: [Card]
  , _playedCards :: Map Color [Card]
  , _discardedCards :: [Card]
  , _hints :: Int
  , _fuckups :: Int
  } deriving (Show)

data Hint
  = ColorHint Color
  | NumberHint Number

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
