{-
  Add a test where X wins by getting all of a column
  Roll out the same solution as we did for Rows and combine the results. Simple :-)
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
rowMap :: Map.Map Move Int
rowMap = Map.fromList [(1,1),(2,1),(3,1),(4,2),(5,2),(6,2),(7,3),(8,3),(9,3)]
colMap :: Map.Map Move Int
colMap = Map.fromList [(1,1),(2,2),(3,3),(4,1),(5,2),(6,3),(7,1),(8,2),(9,3)]
gameEngine :: [Move] -> Result
gameEngine move = maxIdenticalElements rowsTaken `max` maxIdenticalElements colsTaken >= 3
  where
    rowsTaken = map (`Map.lookup` rowMap) (everyOtherElement move)
    colsTaken = map (`Map.lookup` colMap) (everyOtherElement move)
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst(filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))
maxIdenticalElements :: (Ord a) => [a] -> Int
maxIdenticalElements [] = 0
maxIdenticalElements x = maximum (map length (group (Data.List.sort x)))
main :: IO ()
main = putStrLn "Hello World"

