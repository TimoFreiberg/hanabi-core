module Transitions where

import Types
import Game

cardPlayed :: Card -> GameState -> Either GameOver GameState
cardPlayed card = endTurn . tryPlay . removeFromHand card
  where
    tryPlay game =
      if canPlayCard card game
        then (checkHintBonus . putOnPlayedStack card) game
        else putOnDiscardedStack card game
    checkHintBonus
      | isFive card = incrementHintCount
      | otherwise = id

cardDiscarded :: Card -> GameState -> Either GameOver GameState
cardDiscarded card = endTurn . putOnDiscardedStack card . removeFromHand card

hintGiven :: Hint -> PlayerId -> GameState -> GameState
hintGiven hint playerId = decrementHintCount . giveHint hint playerId

endTurn :: GameState -> Either GameOver GameState
endTurn game =
  if any ($ game) [tooManyFailures, allStacksFilled]
    then Left (gameOver game)
    else Right game

gameOver :: GameState -> GameOver
gameOver _ = GameOver 0

maybeIncrementHintCount :: Card -> GameState -> GameState
maybeIncrementHintCount (Card _ Five) = incrementHintCount
maybeIncrementHintCount _ = id
