module CardStack
  ( CardStack
  , cardStack
  , canStack
  , pushCard
  , popCard
  ) where

import Types
import Control.Lens (view)

newtype CardStack =
  CardStack [Card]
  deriving (Show, Eq)

cardStack :: CardStack
cardStack = CardStack []

canStack :: Card -> CardStack -> Bool
canStack card (CardStack cs) =
  case cs of
    [] -> view number card == One
    (c:_) -> card `fitsOn` c

pushCard :: Card -> CardStack -> CardStack
pushCard c (CardStack cs) = CardStack (c : cs)

popCard :: CardStack -> Maybe (Card, CardStack)
popCard (CardStack []) = Nothing
popCard (CardStack (c:cs)) = Just (c, CardStack cs)

fitsOn :: Card -> Card -> Bool
(Card c1 n1) `fitsOn` (Card c2 n2) = c1 == c2 && (n1 `isAfter` n2)

isAfter :: Number -> Number -> Bool
num1 `isAfter` num2 = (num2, num1) `elem` zip allNums (tail allNums)
  where
    allNums = [minBound .. maxBound] :: [Number]
