{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)
import Data.List (intercalate)
import Data.Map.Lazy (Map)
import Data.String (IsString)
import Control.Lens

data Card = Card
    { _color :: Color
    , _number :: Number
    } deriving (Ord,Show,Eq)

data Color
    = White
    | Yellow
    | Green
    | Blue
    | Red
    deriving (Ord,Show,Eq,Bounded,Enum)

data Number
    = One
    | Two
    | Three
    | Four
    | Five
    deriving (Ord,Show,Eq,Bounded,Enum)

instance Arbitrary Number where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Color where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
    arbitrary = do
        color <- arbitrary
        number <- arbitrary
        return (Card color number)

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

data GameState = GameState
    { _actingPlayer :: PlayerId
    , _playerData :: Map PlayerId [Card]
    , _deck :: [Card]
    , _playedCards :: Map Color [Card]
    , _discardedCards :: [Card]
    , _hints :: Int
    , _fuckups :: Int
    } deriving (Show)

newtype PlayerId =
    PlayerId String
    deriving (Eq,Ord,Show,IsString)

data PlayerData =
    PlayerData [(Card, [Clue])]
    deriving (Show,Eq)

data Clue
    = ColorClue Color
    | NumberClue Number
    deriving (Show,Eq)

makeLenses ''GameState
makeLenses ''Card
