module Game where

import GameElements
import Drawing
import Collision

main :: IO ()
main = do
  putStrLn "Enter board dimensions w,h"
  input <- getLine
  let [w,h] = map read (splitOn ',' input) :: [Int]

  putStrLn "Enter x,y,w,h for a black starting block and w,h,color for a block to be placed. Color is interpreted by the first letter."
  let args = splitOn ',' input

  let [board, placedBlocks] = getUserInput w h
  putStrLn "Initial board:"
  draw board

  putStrLn "Possible solutions:"
  sequence_ (map draw (solveGame getBlocks board))
  putStrLn "Done"

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = let (x, rest) = break (== c) s
  in x : case rest of
    [] -> []
    (_:xs) -> splitOn c xs

getBlocks :: [Block]
getBlocks = [Block 3 4 Yellow, Block 3 3 White, Block 2 2 White, Block 2 5 Red, Block 2 4 Red, Block 2 3 Red, Block 1 5 Blue, Block 1 4 Blue]

calcArea :: [Block] -> Int
calcArea blocks = sum (map (\b -> blockHeight b * blockWidth b) blocks)

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