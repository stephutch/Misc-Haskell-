{-
  Test a game when X wins by getting the whole of teh second row.
  Decide we need a more general solution than just checking against specific values for X's moves so need to do some refactoring.
  OK, OK, I should have done the refactoring before a added the new test. Sorry :-(
  Define a map from board positions to the row they're in, then check that all of X's moves are in row 1. 
  All tests pass again (apart from the new one)
  (Haskell note: 'lookup' returns a Maybe value so I can deal with there being no match in the map, that's what all the Just's are for)
-}
module Main (
    main
) where
import qualified Data.Map as Map
-- | TDD As if You Meant it
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
gameEngine move = map (\x -> Map.lookup x rowMap) (everyOtherElement move)  == [Just 1,Just 1,Just 1]
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst(filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))

main :: IO ()
main = putStrLn "Hello World"

