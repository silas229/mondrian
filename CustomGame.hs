module CustomGame where

import GameElements
import Drawing
import StandardGame ( solutions )
import UserInput

--functionality to start a game with fully custom Blocks and Board.

-- Main function
-- Adds the ability to play a custom game with custom blocks and a custom board size
-- Asks the user for input and prints the solutions
main :: IO ()
main = do
  putStrLn "Enter board dimensions w,h:"
  input <- getLine
  let [w,h] = map read (splitOn ',' input) :: [Int]

  game <- getUserInput (Board{boardHeight = h, boardWidth = w, placedBlocks = []}) []
  solutions game


-- presents the user a command line input to create a Game
-- 1. draws supplied board
-- 2. asks user what block to add
-- 3. creates the block, adds it to the board if user wishes to do so
-- 4. call getUserInput with new Board and new Blocklist for next block 
getUserInput :: Board -> [Block] -> IO Game
getUserInput board looseBlocks = do

  let placedBlocksArea = placedBlockArea (placedBlocks board)
  let boardArea = (boardWidth board) * (boardHeight board)
  let looseBlocksArea = blockArea looseBlocks
  let freeArea = boardArea - looseBlocksArea - placedBlocksArea

  if freeArea > 0 then do
    putStrLn "Board: "
    draw board
    putStrLn ("Positions to fill:" ++ show freeArea)

    newBlock <- inputBlock

    putStrLn "Is this block already placed? [y/n]"
    input <- getLine
    if input == "y" then do
        newPlacedBlock <- inputPositionAndRotation newBlock
        getUserInput board{placedBlocks = placedBlocks board ++ [newPlacedBlock]} looseBlocks
      else do
        getUserInput board (looseBlocks ++ [newBlock])

  else if freeArea == 0 then do --board is full, calculate solutions
    return (Game board looseBlocks)

  else do --board is overfilled, reset
    putStrLn "The blocks do not fit on the board. Resetting..."
    getUserInput (board{placedBlocks = []}) []


-- Calculates the cumulative area of all blocks
blockArea :: [Block] -> Int
blockArea blocks = sum (map (\b -> blockHeight b * blockWidth b) blocks)

-- Calculates the cumulative area of all placedBlocks
placedBlockArea :: [PlacedBlock] -> Int
placedBlockArea placedBlocks = blockArea (map block placedBlocks)