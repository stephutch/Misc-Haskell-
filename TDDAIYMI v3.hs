{-
  Test a game which is long enough but no one has won.
  Decide we need a funtion to extract every other element from a list, go and do some TDD to create that
  main solution checks that X has gone in squares 1 then 2 then 3
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
-- >>> gameEngine [1,4,2,5,3]
-- True
-- >>> gameEngine [1,4,2,5,6]
-- False
type Move = Integer
type Result = Bool
gameEngine :: [Move] -> Result
gameEngine move = everyOtherElement move == [1,2,3]
everyOtherElement :: [x] -> [x]
everyOtherElement x = map fst(filter (\ (a,b) -> mod b 2 == 1) (zip x [1..]))
main :: IO ()
main = putStrLn "Hello World"

