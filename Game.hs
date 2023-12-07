
import GameElements
import Drawing

main :: IO ()
main = do
  putStrLn "Enter x,y for block 1x1"
  input <- getLine
  let [x, y] = map read (splitOn ',' input) :: [Int]

  let b1 = Block 1 1 Red
  let p1 = Position x y
  let pb1 = PlacedBlock b1 p1

  putStrLn "Enter x,y,orientation (v/h) for block 1x2"
  input <- getLine
  let [x, y, o] = splitOn ',' input

  let b2 = if o == "v" then
        Block 2 1 Red
      else
        Block 1 2 Red

  let p2 = Position (read x :: Int) (read y :: Int)
  let pb2 = PlacedBlock b2 p2

  putStrLn "Enter x,y,orientation (v/h) for block 1x3"
  input <- getLine
  let [x, y, o] = splitOn ',' input

  let b3 = if o == "v" then
        Block 3 1 Red
      else
        Block 1 3 Red

  let p3 = Position (read x :: Int) (read y :: Int)
  let pb3 = PlacedBlock b3 p3

  let board = Board 8 8 [pb1, pb2, pb3]
  draw board

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = let (x, rest) = break (== c) s
  in x : case rest of
    [] -> []
    (_:xs) -> splitOn c xs

