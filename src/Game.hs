module Game where

import Control.Lens (at, non, over, view, to)
import qualified Data.List as List (delete)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.Random.Shuffle (shuffleM)
import Types

colors :: [Color]
colors = [minBound .. maxBound]

numbers :: [Number]
numbers = [minBound .. maxBound]

occurrences :: Number -> Int
occurrences One = 3
occurrences Two = 2
occurrences Three = 2
occurrences Four = 2
occurrences Five = 1

handSize :: Int -> Int
handSize 2 = 5
handSize 3 = 5
handSize 4 = 4
handSize 5 = 4
handSize n = error ("the game does not support " ++ show n ++ " players!")

maximumFailures :: Int
maximumFailures = 3

sortedGame :: [Card]
sortedGame = concat (concat allCards)
  where
    numberCards col num = replicate (occurrences num) (Card col num)
    colorCards col = map (numberCards col) numbers
    allCards = map colorCards colors

dealCards :: Int -> IO ([[Card]], [Card])
dealCards playerCount = do
  cards <- shuffleM sortedGame
  let (hands', dk) = splitAt (playerCount * handSize playerCount) cards
  let hands = inGroupsOf (handSize playerCount) hands'
  return (hands, dk)

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf _ [] = []
inGroupsOf n xs =
  let (as, bs) = splitAt n xs
  in as : inGroupsOf n bs

-- propNNumbers :: Int -> Number -> Color -> Bool
-- propNNumbers n num col = length ones == n
--   where
--     ones =
--         filter
--             (\(Card c n) ->
--                   c == col && n == num)
--             sortedGame
-- spec :: IO ()
-- spec =
--     hspec $
--     describe "the sorted game" $
--     do it "has three ones" $ property $ propNNumbers 3 One
--        it "has two twos" $ property $ propNNumbers 2 Two
--        it "has two threes" $ property $ propNNumbers 2 Three
--        it "has two fours" $ property $ propNNumbers 2 Four
--        it "has one five" $ property $ propNNumbers 1 Five
initState :: PlayerId -> [PlayerId] -> IO GameState
initState startId ids = do
  (cards, dk) <- dealCards (length cleanIds)
  let hands = fmap createHand cards
  let players = Map.fromList (zip cleanIds hands)
  return (GameState startId players dk Map.empty [] initialHints 0)
  where
    cleanIds = startId : List.delete startId ids

canPlayCard :: Card -> GameState -> Bool
canPlayCard (Card col num1) game =
  case getStack col game of
    [] -> num1 == One
    Card _ num2:_ -> num1 `isSucc` num2

isFive :: Card -> Bool
isFive (Card _ Five) = True
isFive _ = False

tooManyFailures :: GameState -> Bool
tooManyFailures game = view fuckups game >= maximumFailures

allStacksFilled :: GameState -> Bool
allStacksFilled =
  view
    (playedCards .
     to Map.elems . to (fmap length) . to (all (== length numbers)))

getStack :: Color -> GameState -> [Card]
getStack col game = fromMaybe [] (view (playedCards . at col) game)

removeFromHand :: Card -> GameState -> GameState
removeFromHand card game = over handOfActivePlayer (Map.delete card) game
  where
    handOfActivePlayer = playerHands . at (view actingPlayer game) . non Map.empty

putOnPlayedStack :: Card -> GameState -> GameState
putOnPlayedStack card =
  over (playedCards . at (view color card) . non []) (card :)

putOnDiscardedStack :: Card -> GameState -> GameState
putOnDiscardedStack card = over discardedCards (card :)

recordFailure :: GameState -> GameState
recordFailure = over fuckups (+ 1)

incrementHintCount :: GameState -> GameState
incrementHintCount = over hints (+ 1)

decrementHintCount :: GameState -> GameState
decrementHintCount = over hints (subtract 1)

giveHint hint player =
  over (playerHands . at player . non Map.empty) (applyHint hint)

applyHint (ColorHint col) hand = Map.mapWithKey

applyHint (NumberHint num) hand
