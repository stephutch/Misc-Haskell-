{-
  Check the other diagonal, and now onto . . . O winning!
  Solution is easy, whip the first element off the list, O becomes X and apply the same logic. 
-}module Main (
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
-- False
-- >>> gameEngine [1,3,2]
-- False
-- >>> gameEngine [1]
-- False
-- >>> gameEngine [1,4,2,5,3]
-- True
-- >>> gameEngine [1,4,2,5,6]
-- False
-- >>> gameEngine [4,1,5,2,6]
-- True
-- >>> gameEngine [7,1,8,2,9]
-- True
-- >>> gameEngine [1,2,4,3,7]
-- True
-- >>> gameEngine [2,1,5,3,8]
-- True
-- >>> gameEngine [3,1,6,3,9]
-- True
-- >>> addDiags [1,1]
-- [1,1,12,0]
-- >>> addDiags [2,3]
-- [2,3,15,-1]
-- >>> gameEngine [1,2,5,3,9]
-- True
-- >>> gameEngine [3,2,5,4,7]
-- True
-- >>> gameEngine [1,3,2,5,4,7]
-- True
type Move = Integer
type Result = Bool
rowColMap :: Map.Map Move [Int]
rowColMap = Map.fromList [(1,[1,1]),(2,[1,2]),(3,[1,3]),(4,[2,1]),(5,[2,2]),(6,[2,3]),(7,[3,1]),(8,[3,2]),(9,[3,3])]
gameEngine :: [Move] -> Result
gameEngine [] = False
gameEngine [a] = False  -- so we don;t fall over when we do "tail moves"
gameEngine moves = gameEngine' moves || gameEngine' (tail moves)
gameEngine' moves = maximum (map maxIdenticalElements (transpose rowColsTaken)) >= 3
  where
    rowColsTaken :: [[Int]]
    rowColsTaken = map (addDiags.fromJust.(`Map.lookup` rowColMap)) (everyOtherElement moves)
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst(filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))
maxIdenticalElements :: (Ord a) => [a] -> Int
maxIdenticalElements [] = 0
maxIdenticalElements x = maximum (map length (group (Data.List.sort x)))
addDiags :: [Int] -> [Int]
addDiags [a,b] = [a,b,a+b+10,a-b]
main :: IO ()
main = putStrLn "Hello World"