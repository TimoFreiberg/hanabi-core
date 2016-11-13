module Transitions where

import Types
import Game

cardPlayed :: Card -> Game -> Either GameOver Game
cardPlayed card = endTurn . drawCard . tryPlay . removeFromHand card
  where
    tryPlay game =
      if canPlayCard card game
        then (checkHintBonus . putOnPlayedStack card) game
        else putOnDiscardedStack card game
    checkHintBonus
      | isFive card = incrementHintCount
      | otherwise = id

cardDiscarded :: Card -> Game -> Either GameOver Game
cardDiscarded card =
  endTurn . drawCard . putOnDiscardedStack card . removeFromHand card

hintGiven :: Hint -> PlayerId -> Game -> Either GameOver Game
hintGiven hint playerId = endTurn . decrementHintCount . giveHint hint playerId

endTurn :: Game -> Either GameOver Game
endTurn game =
  if any ($ game) [tooManyFailures, allStacksFilled]
    then Left (gameOver game)
    else Right (nextPlayer game)

gameOver :: Game -> GameOver
gameOver game = GameOver (getScore game)

maybeIncrementHintCount :: Card -> Game -> Game
maybeIncrementHintCount (Card _ Five) = incrementHintCount
maybeIncrementHintCount _ = id
