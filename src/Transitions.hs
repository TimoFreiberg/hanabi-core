module Transitions where

import Types
import Game

cardPlayed :: Card -> Game -> Either GameOver Game
cardPlayed card = endTurn . tryPlay . removeFromHand card
  where
    tryPlay game =
      if canPlayCard card game
        then (checkHintBonus . putOnPlayedStack card) game
        else putOnDiscardedStack card game
    checkHintBonus
      | isFive card = incrementHintCount
      | otherwise = id

cardDiscarded :: Card -> Game -> Either GameOver Game
cardDiscarded card = endTurn . putOnDiscardedStack card . removeFromHand card

hintGiven :: Hint -> PlayerId -> Game -> Game
hintGiven hint playerId = decrementHintCount . giveHint hint playerId

endTurn :: Game -> Either GameOver Game
endTurn game =
  if any ($ game) [tooManyFailures, allStacksFilled]
    then Left (gameOver game)
    else Right game

gameOver :: Game -> GameOver
gameOver _ = GameOver 0

maybeIncrementHintCount :: Card -> Game -> Game
maybeIncrementHintCount (Card _ Five) = incrementHintCount
maybeIncrementHintCount _ = id
