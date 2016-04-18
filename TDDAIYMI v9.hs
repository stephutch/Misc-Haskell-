{-
  Some refactoring
  Decide that the two seperate lists for rows and columns is a bit tacky, so combine them into a single row/colum map.
  Partly I did this because I was thinking ahead to having to deal with diagonals. Having two lists for row and column is not too bad, but four lists for row, col, -ve diagonal and -ve diagonal would be really tacky.
  This is where I slightly stuggle with TDDAIYMI. Normally, I'd do refactoring driven by the need to sort a failing test. In TDDAIYMI I have to either do it before I add the test 'in anticipation' or after I've got the test working to sort out the tacky code I've had to write.
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

type Move = Integer
type Result = Bool
rowColMap :: Map.Map Move [Int]
rowColMap = Map.fromList [(1,[1,1]),(2,[1,2]),(3,[1,3]),(4,[2,1]),(5,[2,2]),(6,[2,3]),(7,[3,1]),(8,[3,2]),(9,[3,3])]
gameEngine :: [Move] -> Result
gameEngine [] = False
gameEngine move = maximum (map maxIdenticalElements (transpose rowColsTaken)) >= 3
  where
    rowColsTaken :: [[Int]]
    rowColsTaken = map (fromJust.(`Map.lookup` rowColMap)) (everyOtherElement move)
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst(filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))
maxIdenticalElements :: (Ord a) => [a] -> Int
maxIdenticalElements [] = 0
maxIdenticalElements x = maximum (map length (group (Data.List.sort x)))
main :: IO ()
main = putStrLn "Hello World"

