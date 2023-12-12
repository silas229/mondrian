module StandardGame where

import GameElements
import Drawing
import Solving
import UserInput

--functionality to start a normal Mondrian Blocks game with the normal Blocks and Board.


-- Main function
-- Provides the ability to solve a normal Mondrian Blocks game
-- Asks the user for the positions of the three black blocks and prints the solutions
main :: IO ()
main = do
  let blackBlocks = [Block 1 1 Black, Block 2 1 Black, Block 3 1 Black]
  placedBlackBlocks <- mapM inputPositionAndRotation blackBlocks

  let board = Board 8 8 placedBlackBlocks
  putStrLn "Initial board:"
  draw board

  solutions (Game board getBlocks)

-- first prints all solutions for the Game to the console
-- then lets the user select any solution by index to print again
solutions :: Game -> IO ()
solutions game = do
  putStrLn "Possible solutions:"
  let solutions = solveGame (blocks game) (board game)
  sequence_ (map draw solutions)
  putStrLn ("Done. Number of solutions: " ++ show (length solutions))
  if length solutions > 1 then 
    loopPrintSelectSolution solutions 
    else return()
  where
    loopPrintSelectSolution :: [Board] -> IO()
    loopPrintSelectSolution solutions = do
      putStrLn "type index of solution to print or q to leave."
      input <- getLine
      if input /= "q" then do
          let index = read input :: Int
          draw (solutions!!index)
          loopPrintSelectSolution solutions
        else return()


-- all freely placable blocks in the original Mondrian Blocks game
getBlocks :: [Block]
getBlocks = [Block 3 4 Yellow, Block 3 3 White, Block 2 2 White, Block 2 5 Red, Block 2 4 Red, Block 2 3 Red, Block 1 5 Blue, Block 1 4 Blue]