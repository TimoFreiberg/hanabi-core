module Game
  (initState,canStack,putInStack)
  where

import System.Random.Shuffle (shuffleM)
import qualified Data.Map.Strict as Map
import qualified Data.List as List (delete)
import Control.Lens(over,at,view,non)

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

sortedGame :: [Card]
sortedGame = concat (concat allCards)
  where
    numberCards col num =
        replicate (occurrences num) (Card col num)
    colorCards col = map (numberCards col) numbers
    allCards = map colorCards colors

setup :: Int -> IO ([[Card]], [Card])
setup playerCount = do
    cards <- shuffleM sortedGame
    let (hands',dk) = splitAt (playerCount * handSize playerCount) cards
    let hands = inGroupsOf (handSize playerCount) hands'
    return (hands, dk)

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf _ [] = []
inGroupsOf n xs =
    let (as,bs) = splitAt n xs
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

initialHints :: Int
initialHints = 7

initState :: PlayerId -> [PlayerId] -> IO GameState
initState startId ids = do
    (cards,dk) <- setup (length cleanIds)
    let players = Map.fromList (zip cleanIds cards)
    return (GameState startId players dk Map.empty [] initialHints 0)
  where
    cleanIds = startId : List.delete startId ids

canStack :: Card -> Card -> Bool
canStack (Card c1 n1) (Card c2 n2) = c1 == c2 && (fromEnum n1 - fromEnum n2) == 1

putInStack :: Card -> GameState -> GameState
putInStack card = over (playedCards . at (view color card) . non []) (card:)
