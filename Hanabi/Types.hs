{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hanabi.Types where

import Data.List (intercalate)
import Data.String (IsString)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function (on)
import GHC.Generics

data Card = Card
  { _color :: Color
  , _number :: Number
  } deriving (Show, Eq, Ord, Generic)

data Color
  = White
  | Yellow
  | Green
  | Blue
  | Red
  deriving (Ord, Show, Eq, Bounded, Enum, Generic)

data Number
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Ord, Show, Eq, Bounded, Enum, Generic)

newtype GameOver =
  GameOver Int
  deriving (Ord, Show, Eq, Generic)

isSuccessor
  :: (Bounded t, Enum t, Eq t)
  => t -> t -> Bool
num1 `isSuccessor` num2 = (num1, num2) `elem` zip allNums (tail allNums)
  where
    allNums = [minBound .. maxBound]

makeLenses ''Card

newtype PlayerId =
  PlayerId Text
  deriving (Eq, Ord, Show, IsString, Generic)

type Hand = [(Card, Set Fact)]

getCards :: Hand -> [Card]
getCards = map fst

createHand :: [Card] -> Hand
createHand = fmap (, Set.empty)

data Fact
  = IsColor Color
  | IsNumber Number
  | Not Fact
  deriving (Show, Eq, Ord, Generic)

isPositiveFact :: Fact -> Bool
isPositiveFact (Not _) = False
isPositiveFact _ = True

isNumberFact :: Fact -> Bool
isNumberFact (Not (IsNumber _)) = True
isNumberFact (IsNumber _) = True
isNumberFact _ = False

differentType :: Fact -> Fact -> Bool
differentType = (/=) `on` isNumberFact

isColorFact :: Fact -> Bool
isColorFact = not . isNumberFact

data Game = Game
  { _actingPlayer :: PlayerId
  , _playerHands :: Map PlayerId Hand
  , _deck :: [Card]
  , _playedCards :: Map Color [Card]
  , _discardedCards :: [Card]
  , _hints :: Int
  , _fuckups :: Int
  , _lastPlayer :: Maybe PlayerId
  } deriving (Show, Generic)

data Hint
  = ColorHint Color
  | NumberHint Number
  deriving (Show, Generic)

class IsHint a  where
  toHint :: a -> Hint

instance IsHint Hint where
  toHint = id

instance IsHint Color where
  toHint = ColorHint

instance IsHint Number where
  toHint = NumberHint

makeLenses ''Game

initialHints :: Int
initialHints = 7

prettyPrint
  :: Pprint a
  => a -> IO ()
prettyPrint = putStrLn . pprint

fairPrint :: Game -> IO ()
fairPrint (Game actingPlayer' playerHands' _ playedCards' discardedCards' hints' fuckups' lastPlayer') =
  putStrLn
    (intercalate
       "\n"
       [ "Active: " ++ pprint actingPlayer'
       , ""
       , "Hands:"
       , concat
           [ pprint pId ++
            ":\n" ++
            concat
              [ show i ++
               ". " ++
               (if pId == actingPlayer'
                  then ""
                  else pprint card) ++
               " " ++ pprint facts ++ "\n"
              | (i, (card, facts)) <- zip [0 :: Int ..] hand ] ++
            "\n"
           | (pId, hand) <- Map.assocs playerHands' ]
       , "Played Cards:"
       , pprint playedCards'
       , "Discarded Cards:"
       , "  " ++ pprint discardedCards'
       , "hints: " ++ show hints'
       , "fuckups: " ++ show fuckups'
       , case lastPlayer' of
           Nothing -> ""
           Just lastPlayerId -> "\nLast Player: " ++ show lastPlayerId
       ])

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
  pprint = pprint . Set.elems

instance (Pprint k, Pprint v) =>
         Pprint (Map k v) where
  pprint m =
    concat
      [ pprint k ++ ":" ++ pprint v ++ "\n"
      | (k, v) <- Map.assocs m ]

instance Pprint Game where
  pprint (Game actingPlayer' playerHands' deck' playedCards' discardedCards' hints' fuckups' lastPlayer') =
    intercalate
      "\n"
      [ "Active: " ++ pprint actingPlayer'
      , ""
      , "Hands:"
      , concat
          [ pprint pId ++
           ":\n" ++
           concat
             [ show i ++ ". " ++ pprint card ++ " " ++ pprint facts ++ "\n"
             | (i, (card, facts)) <- zip [0 :: Int ..] hand ] ++
           "\n"
          | (pId, hand) <- Map.assocs playerHands' ]
      , "Played Cards:"
      , pprint playedCards'
      , "Discarded Cards:"
      , "  " ++ pprint discardedCards'
      , "hints: " ++ show hints'
      , "fuckups: " ++ show fuckups'
      , case lastPlayer' of
          Nothing -> ""
          Just lastPlayerId -> "\nLast Player: " ++ show lastPlayerId
      , "Deck:"
      , pprint deck'
      ]
