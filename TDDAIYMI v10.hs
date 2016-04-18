{-
  Having Result be just a boolean is probably not teh full requirements
  Change Result to be an enumerated type (Incomplete | XWins | OWins | Draw) and make the test expect the relevent result.
  I little jiggling of the code and were there.
  Any suggestion that I fixed the code first and then corrected the tests will be strenously denied   
-}
module Main (
    main
) where
import qualified Data.Map as Map
import Data.List (sort, group, transpose)
import Data.Maybe (fromJust)
-- | TDD As if You Meant it
-- >>> maxIdenticalElements []
-- 0
-- >>> maxIdenticalElements [1]
-- 1
-- >>> maxIdenticalElements [1,1]
-- 2
-- >>> maxIdenticalElements [1,1,2]
-- 2
-- >>> everyOtherElement []
-- []
-- >>> everyOtherElement [1]
-- [1]
-- >>> everyOtherElement [1,2]
-- [1]
-- >>> everyOtherElement [1,8,3,7,2,5]
-- [1,3,2]
-- >>> gameEngine []
-- Incomplete
-- >>> gameEngine [1,3,2]
-- Incomplete
-- >>> gameEngine [1]
-- Incomplete
-- >>> gameEngine [1,4,2,5,3]
-- XWins
-- >>> gameEngine [1,4,2,5,6]
-- Incomplete
-- >>> gameEngine [4,1,5,2,6]
-- XWins
-- >>> gameEngine [7,1,8,2,9]
-- XWins
-- >>> gameEngine [1,2,4,3,7]
-- XWins
-- >>> gameEngine [2,1,5,3,8]
-- XWins
-- >>> gameEngine [3,1,6,3,9]
-- XWins
-- >>> addDiags [1,1]
-- [1,1,12,0]
-- >>> addDiags [2,3]
-- [2,3,15,-1]
-- >>> gameEngine [1,2,5,3,9]
-- XWins
-- >>> gameEngine [3,2,5,4,7]
-- XWins
-- >>> gameEngine [1,3,2,5,4,7]
-- OWins
type Move = Integer
data Result = Incomplete | XWins | OWins | Draw deriving (Show)
rowColMap :: Map.Map Move [Int]
rowColMap = Map.fromList [(1,[1,1]),(2,[1,2]),(3,[1,3]),(4,[2,1]),(5,[2,2]),(6,[2,3]),(7,[3,1]),(8,[3,2]),(9,[3,3])]
gameEngine :: [Move] -> Result
gameEngine [] = Incomplete
gameEngine [a] = Incomplete  -- so we don;t fall over when we do "tail moves"
gameEngine moves
     | firstPlayerWon moves = XWins  -- assume X plays first
     | firstPlayerWon (tail moves) = OWins
     | length moves >= 9 = Draw
     | otherwise = Incomplete
firstPlayerWon moves = maximum (map maxIdenticalElements (transpose rowColsTaken)) >= 3
  where
    rowColsTaken :: [[Int]]
    rowColsTaken = map (addDiags.fromJust.(`Map.lookup` rowColMap)) (everyOtherElement moves)
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst (filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))
maxIdenticalElements :: (Ord a) => [a] -> Int
maxIdenticalElements [] = 0
maxIdenticalElements x = maximum (map length (group (Data.List.sort x)))
addDiags :: [Int] -> [Int]
addDiags [a,b] = [a,b,a+b+10,a-b] -- add, say, 10 to the a+b values to stop them overlapping with th a-b values
main :: IO ()
main = putStrLn "Hello World"

