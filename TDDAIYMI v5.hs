{-
  So, back to fixing the test of a game when X wins by getting the whole of the second row.
  map X's moves to the row they're in and apply to it a funtion that returns the maximum number of identical elements in a list. If the answer's 3 we've got 3 moves in the same row - Bingo.
  This change also solves the problem that the moves can come in any order. I ought to probably have a test for that. I've killed two birds with one stone, but only tested for one bird.
  If I'd takled the move order problem first, I'd have probably done it by sorting X's moves. I'd then have thrown away the sort when I'd done this solution.
-}
module Main (
    main
) where
import qualified Data.Map as Map
import Data.List (sort, group)
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
type Move = Integer
type Result = Bool
rowMap :: Map.Map Move Int
rowMap = Map.fromList [(1,1),(2,1),(3,1),(4,2),(5,2),(6,2),(7,3),(8,3),(9,3)]
gameEngine :: [Move] -> Result
gameEngine move = maxIdenticalElements rowsTaken >= 3
  where
    rowsTaken = map (`Map.lookup` rowMap) (everyOtherElement move)
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst(filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))
maxIdenticalElements :: (Ord a) => [a] -> Int
maxIdenticalElements [] = 0
maxIdenticalElements x = maximum (map length (group (Data.List.sort x)))
main :: IO ()
main = putStrLn "Hello World"

