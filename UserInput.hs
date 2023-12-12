module UserInput where
import GameElements
import Data.Char (toUpper)

--functionality for the user to input various game elements via the console.


--lets the user input a Block via the command line.
inputBlock :: IO Block
inputBlock = do
    putStrLn "Enter w,h,color of the block to place. Choose color from {s, r, y, b, w}."
    [h,w,c] <- getInput ',' 3

    let color = getColor c 
    case color of
        Nothing -> do
            putStrLn "Invalid color. Try again."
            inputBlock
        Just color -> return (Block (read h :: Int) (read w :: Int) color)

getColor :: String -> Maybe Color
getColor color = case color of
    "s" -> Just Black
    "r" -> Just Red
    "y" -> Just Yellow
    "b" -> Just Blue
    "w" -> Just White
    _ -> Nothing


--lets the user input a position and orientation for the given Block.
--creates the corresponding PlacedBlock.
inputPositionAndRotation :: Block -> IO PlacedBlock
inputPositionAndRotation block = do
    let longSide = max (blockHeight block) (blockWidth block)
    let shortSide = min (blockHeight block) (blockWidth block)

    putStrLn ("Enter x,y,orientation (v/h) for block " ++ show shortSide ++ "x" ++ show longSide)
    [x, y, o] <- getInput ',' 3

    let rotatedBlock = if o == "v" then
            Block longSide shortSide (color block)
        else
            Block shortSide longSide (color block)

    return PlacedBlock{topLeftCorner = Position (read x :: Int) (read y :: Int), block = rotatedBlock}

--get certain number of Strings separated by a delimiter
getInput :: Char -> Int -> IO [String]
getInput delimiter count = do
    input <- getLine
    let arguments = splitOn delimiter input
    if length arguments == count
        then return arguments
        else do
            putStrLn "Wrong number of arguments. Try again"
            getInput delimiter count


-- Splits a string on a given character
-- Returns a list of strings
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = let (x, rest) = break (== c) s
  in x : case rest of
    [] -> []
    (_:xs) -> splitOn c xs