module Drawing where

import GameElements
import Solving

------------- functions to draw a Board to the Console


--draws the Board in the Console. Coordinates occupied by a Block are marked with an X, the rest with an O
draw :: Board -> IO()
draw board = do
    (Board h w blocks) <- return board
    let emptyStrings = replicate (h*w) " "
    let blockedPositions = listOccupiedpositions blocks
    let blockedPositionInts = map (positionToInt board) blockedPositions

    let filledString = multiReplaceAt emptyStrings board blockedPositions
    let formattedString = concat (multiInsertAt ["\n"] filledString [ x*w | x <- reverse [1..h-1] ])
    putStrLn formattedString
    putStrLn (replicate w '-')

-- returns the Color attribute of the PlacedBlock
colorOf :: PlacedBlock -> Color
colorOf (PlacedBlock (Block _ _ color) _) = color


colorString :: Color -> String
colorString color = getColorCode color ++ "█" ++ getColorCode Default

-- returns all PlacedBlocks that occupy a single position on the Board
-- if the game is played correctly, there should not be more than one block in one Position, but it can happen.
placedBlocksAt :: Position -> Board -> [PlacedBlock]
placedBlocksAt position (Board h w blocks) = filter (occupies position) blocks

-- returns the 'Just'+color at the position on the Board or 'Nothing'
colorAt :: Position -> Board -> Maybe Color
colorAt position board =
    if length (placedBlocksAt position board) > 0
    then case placedBlocksAt position board of
           [] -> Nothing
           (x:_) -> Just (colorOf x)
    else Nothing

positionToInt :: Board -> Position -> Int
positionToInt (Board h w _) (Position x y) = y * w + x

-- returns all Positions occupied by the PlacedBlocks. May contain duplicates if two PlacedBlocks overlap.
listOccupiedpositions :: [PlacedBlock] -> [Position]
listOccupiedpositions placedBlocks = concat (map allOccupiedPositionsPlacedBlock placedBlocks)

-- inserts the element into the original list at the supplied index with toInsert - the returned list will therefore contain one more element than original
insertAt :: [a] -> [a] -> Int -> [a]
insertAt toInsert original index = do
    let( start , end ) = splitAt index original
    start ++ toInsert ++ end

-- uses insertAt to recursively insert toInsert into the original List at all supplied indices
multiInsertAt :: [a] -> [a] -> [Int] -> [a]
multiInsertAt _ original [] = original
multiInsertAt toInsert original (indicesHead:indicesTail) =
    multiInsertAt toInsert (insertAt toInsert original indicesHead) indicesTail

-- replaces the element in the original list at the supplied index with toInsert
replaceAt :: String -> [String] -> Int -> [String]
replaceAt toInsert original index = do
    let( start , _:end ) = splitAt index original
    start ++ [toInsert] ++ end

-- uses replaceAt to recursively replace the elements in the original List at all supplied indices with toInsert
multiReplaceAt :: [String] -> Board -> [Position] -> [String]
multiReplaceAt original board [] = original
multiReplaceAt original board (indicesHead:indicesTail) =
    let placedBlocks = placedBlocksAt indicesHead board
    in case placedBlocks of
        [] -> multiReplaceAt original board indicesTail
        (x:_) -> multiReplaceAt (replaceAt (colorString (colorOf x)) original (positionToInt board indicesHead)) board indicesTail

-- returns the ANSI color code for the supplied Color
getColorCode :: Color -> String
getColorCode color = case color of
    Default -> "\ESC[0m"
    Black -> "\ESC[30m"
    Red -> "\ESC[31m"
    Yellow -> "\ESC[33m"
    Blue -> "\ESC[34m"
    White -> "\ESC[37m"
