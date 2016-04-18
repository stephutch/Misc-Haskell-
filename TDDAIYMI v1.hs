{-
TTD As IF You Meant It in Haskell - Stephen Hutchinson

My Skill Level
  I've done TDD before, but I'm just learning Haskell so some of the code may be a bit ropey

My environment: 
 I'm using Leksah for my IDE, I tries Eclipse but it was not stable enough. Leksah is a little tacky round the edges but it does work.
 I'm using doctest (modelled on Python's doctest)for my testing so the tests are in the comments. Again, maybe not my first choice, but it does work.
 
TDDAIYMI in Haskel
  The rule about when you can create classes doesn't really apply as Haskell isn't an OO language
  As all the tests are just expressions with expected values, I could see how the whole bit of only writeing new code in the test cases applied. Perhaps I should have tried harder, bbut you don't get the automated syntax checking in the test code (because it's in a comment) so it seemed rather perverse to write new code there.
  The main program (Hello World) is just there for form's sake. It doesn't for part of the solution.
  
Step one
  Decide that the requirement is a function, lets call it 'gameEngine', that takes a set of moves, being a list of integers, and rturns True if someone has won, and False otherwise. The integers represent positions on the boards numbered 1-9 and players alternate turns
  First simple test, if no one has played no one has won.
  The types Move and Result are not strictly needed but count, I think, as Good Style.
-}
module Main (
    main
) where
-- | TDD As If You Meant It
-- >>> gameEngine []
-- False
type Move = Integer
type Result = Bool
gameEngine :: [Move] -> Result
gameEngine _ = False

main :: IO ()
main = putStrLn "Hello World"

