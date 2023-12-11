module Game where

import GameElements
import Drawing
import Collision

main :: IO ()
main = do
  putStrLn "Enter x,y for block 1x1"
  input <- getLine
  let [x, y] = map read (splitOn ',' input) :: [Int]

  let b1 = Block 1 1 Black
  let p1 = Position x y
  let pb1 = PlacedBlock b1 p1

  putStrLn "Enter x,y,orientation (v/h) for block 1x2"
  input <- getLine
  let [x, y, o] = splitOn ',' input

  let b2 = if o == "v" then
        Block 2 1 Black
      else
        Block 1 2 Black

  let p2 = Position (read x :: Int) (read y :: Int)
  let pb2 = PlacedBlock b2 p2

  putStrLn "Enter x,y,orientation (v/h) for block 1x3"
  input <- getLine
  let [x, y, o] = splitOn ',' input

  let b3 = if o == "v" then
        Block 3 1 Black
      else
        Block 1 3 Black

  let p3 = Position (read x :: Int) (read y :: Int)
  let pb3 = PlacedBlock b3 p3

  let board = Board 8 8 [pb1, pb2, pb3]
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