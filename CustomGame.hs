module CustomGame where

import GameElements
import Drawing
import Collision
import Game

-- Main function
-- Adds the ability to play a custom game with custom blocks and a custom board size
-- Asks the user for input and prints the solutions
main :: IO ()
main = do
  putStrLn "Enter board dimensions w,h"
  input <- getLine
  let [w,h] = map read (splitOn ',' input) :: [Int]

  putStrLn "Enter x,y,w,h for a black starting block and w,h,color for a block to be placed. Position 0,0 is at the upper left corner. Color is interpreted by the first letter."
  let args = splitOn ',' input

  let game = getUserInput w h
  putStrLn "Initial board:"
  draw (board game)

  solutions game

-- Calculates the cumulative area of all blocks
calcArea :: [Block] -> Int
calcArea blocks = sum (map (\b -> blockHeight b * blockWidth b) blocks)

-- Get user input for the blocks (3 args) and placed blocks (4 args)
-- Returns Game data type which consists of a tuple of the Board and the Blocks
-- Returns Nothing if the blocks do not fit on the board
getUserInput :: Int -> Int ->IO (Maybe [Game])
getUserInput w h = do
  let blocks = []
  let pblocks = []
  let area = 0
  if (w*h > area) then do
    putStrLn "Positions to fill:" ++ show (w*h - area)
    input <- getLine
    let args = splitOn ',' input
    if length args == 3 then do
      let [w,h,color] = args
      blocks ++ Block (read w :: Int) (read h :: Int) (getColor toUpper (read color :: Char))
    else if length args == 4 then do
      let [x,y,w,h] = args
      let b = Block (read w :: Int) (read h :: Int) Black
      pblocks ++ placeBlock b (read x :: Int) (read y :: Int)
    else do
      putStrLn "Invalid input. Please try again."
    calcArea (blocks ++ map (\pb -> block pb) pbs)
    getUserInput
  else if w*h == area then do
    return [Board w h pbs, blocks]
  else do
    putStrLn "The blocks do not fit on the board. Please enter a bigger board or smaller blocks"
    return Nothing