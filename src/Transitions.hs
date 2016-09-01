module Transitions where

import Types
import Game

import Control.Lens

type Transition a = a -> GameState -> GameState

cardPlayed :: Transition Card
cardPlayed card game
  | cardFits = over playedCards (play card) game
  | otherwise = over discardedCards (card :) game
  where
    cardFits = onFirstElement (canStack card) existingStack
    existingStack = (view (playedCards . at (view color card)) game)
    play = undefined

onFirstElement :: (a -> Bool) -> Maybe [a] -> Bool
onFirstElement _ Nothing = True
onFirstElement _ (Just []) = True
onFirstElement f (Just (x:_)) = f x
