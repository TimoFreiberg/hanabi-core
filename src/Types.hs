{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.List (intercalate)
import Data.String (IsString)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

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
  deriving (Ord, Show, Eq)

isSucc
  :: (Bounded t, Enum t, Eq t)
  => t -> t -> Bool
num1 `isSucc` num2 = (num1, num2) `elem` zip allNums (tail allNums)
  where
    allNums = [minBound .. maxBound]

makeLenses ''Card

newtype PlayerId =
  PlayerId Text
  deriving (Eq, Ord, Show, IsString)

type Hand = Map Card (Set Fact)

getCards :: Hand -> [Card]
getCards = Map.keys

createHand :: [Card] -> Hand
createHand cards = Map.fromList (fmap (, Set.empty) cards)

data Fact
  = IsColor Color
  | IsNumber Number
  | Not Fact
  deriving (Show, Eq, Ord)

data Game = Game
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

makeLenses ''Game

initialHints :: Int
initialHints = 7

prettyPrint
  :: Pprint a
  => a -> IO ()
prettyPrint = putStrLn . pprint

class Pprint a  where
  pprint :: a -> String

instance Pprint Color where
  pprint = take 1 . show

instance Pprint Number where
  pprint = show . (+ 1) . fromEnum

instance Pprint Fact where
  pprint (Not f) = "!" ++ pprint f
  pprint (IsColor c) = pprint c
  pprint (IsNumber n) = pprint n

instance Pprint Card where
  pprint (Card col num) = "(" ++ pprint col ++ " " ++ pprint num ++ ")"

instance Pprint Text where
  pprint = Text.unpack

instance Pprint PlayerId where
  pprint (PlayerId s) = "Player " ++ pprint s

instance Pprint a =>
         Pprint [a] where
  pprint xs = concat ["[", intercalate ", " (map pprint xs), "]"]

instance Pprint a =>
         Pprint (Set a) where
  pprint = Set.foldl' (\str a -> str ++ pprint a) ""

instance (Pprint k, Pprint v) =>
         Pprint (Map k v) where
  pprint m =
    concat
      [ pprint k ++ ":" ++ pprint v ++ "\n"
      | (k, v) <- Map.assocs m ]

instance Pprint Game where
  pprint (Game actingPlayer' playerHands' deck' playedCards' discardedCards' hints' fuckups') =
    intercalate
      "\n"
      [ "Active: " ++ pprint actingPlayer'
      , ""
      , "Hands:"
      , concat
          [ pprint pId ++
           ":\n" ++
           concat
             [ "  " ++ pprint card ++ " " ++ pprint facts ++ "\n"
             | (card, facts) <- Map.assocs hand ] ++
           "\n"
          | (pId, hand) <- Map.assocs playerHands' ]
      , "Played Cards:"
      , pprint playedCards'
      , "Discarded Cards:"
      , "  " ++ pprint discardedCards'
      , "hints: " ++ show hints'
      , "fuckups: " ++ show fuckups'
      , ""
      , "Deck:"
      , pprint deck'
      ]
