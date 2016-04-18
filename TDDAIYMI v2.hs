{-
  Next tests, including adding a game where someone wins.
  The solution simply checks the game is long enough for someone to have won
-}
module Main (
    main
) where

-- | TDD As if You Meant it
-- >>> gameEngine []
-- False
-- >>> gameEngine [1]
-- False
-- >>> gameEngine [1,4,2,5,3]
-- True
type Move = Integer
type Result = Bool
gameEngine :: [Move] -> Result
gameEngine move = length move  > 4
main :: IO ()
main = putStrLn "Hello World"

