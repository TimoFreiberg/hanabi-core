module Transitions where

import Types
import GameState
import Game

import Control.Lens

type Transition a = a -> GameState -> GameState

playCard card game = if cardFits then putCardOnStack else discardCard <> failure
  where
    cardFits = card `canStackOn` (stackWithColor (view color card))
    stackWithColor color = view (playedCards . at color) game
    canStackOn c = maybe True (canStack c)
    
    
discardCard = undefined
giveHint = undefined

cardPlayed :: Transition Card
cardPlayed card game
  | cardFits = over playedCards (play card) game
  | otherwise = over discardedCards (card :) game
  where
    cardFits = onFirstElement (canStack card) existingStack
    existingStack = view (playedCards . at (view color card)) game
    play = undefined

onFirstElement :: (a -> Bool) -> Maybe [a] -> Bool
onFirstElement _ Nothing = True
onFirstElement _ (Just []) = True
onFirstElement f (Just (x:_)) = f x
