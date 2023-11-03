module Drawing where

import GameElements
import Collision


-- TODO: look into https://hackage.haskell.org/package/brick

------------- functions to draw a Board to the Console
--draws the Board in the Console. Coordinates occupied by a Block are marked with an X, the rest with an O
draw :: Board -> IO()
draw (Board h w blocks) = do
    let emptyString = replicate (h*w) 'O'
    let blockedPositions = listOccupiedpositions blocks
    let blockedPositionInts = map (positionToInt (Board h w blocks)) blockedPositions
    let filledString = multiReplaceAt "X" emptyString blockedPositionInts
    let formattedString = multiInsertAt "\n" filledString [ x*w | x <- reverse [1..h-1] ]
    putStrLn formattedString

positionToInt :: Board -> Position -> Int
positionToInt (Board h w _) (Position x y) = y * w + x

-- returns all Positions occupied by the PlacedBlocks. May contain duplicates if two PlacedBlocks overlap.
listOccupiedpositions :: [PlacedBlock] -> [Position]
listOccupiedpositions placedBlocks = concat (map occupiedPositions placedBlocks)

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
replaceAt :: [a] -> [a] -> Int -> [a]
replaceAt toInsert original index = do
    let( start , _:end ) = splitAt index original
    start ++ toInsert ++ end

-- uses replaceAt to recursively replace the elements in the original List at all supplied indices with toInsert
multiReplaceAt :: [a] -> [a] -> [Int] -> [a]
multiReplaceAt _ original [] = original
multiReplaceAt toInsert original (indicesHead:indicesTail) =
    multiReplaceAt toInsert (replaceAt toInsert original indicesHead) indicesTail