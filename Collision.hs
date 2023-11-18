module Collision where

import GameElements


solveGame :: [Block] -> Board -> [Board]
solveGame [] board = [board]
solveGame (nextBlock:restBlocks) board = concat (map (allPossiblePlacements nextBlock) (solveGame restBlocks board))

allPossiblePlacements :: Block -> Board -> [Board]
allPossiblePlacements block board = placeOnPositionsIfPossible block board (allPositionsInBounds board) []

placeOnPositionsIfPossible :: Block -> Board -> [Position] -> [Board] -> [Board]
placeOnPositionsIfPossible _ _ [] solutions = solutions
placeOnPositionsIfPossible block originalBoard (nextPosition:restPositions) alreadyFoundSolutions = do
    let placedBlock = PlacedBlock{block=block, topLeftCorner=nextPosition}
    let isAbleToPlace = not (isInBounds originalBoard placedBlock) || isColliding originalBoard placedBlock
    let nowFoundSolutions = if isColliding originalBoard placedBlock 
        then alreadyFoundSolutions
        else alreadyFoundSolutions ++ [placeBlock originalBoard placedBlock]
    placeOnPositionsIfPossible block originalBoard restPositions nowFoundSolutions

allPositionsInBounds :: Board -> [Position]
allPositionsInBounds (Board height width _) = [Position{x, y} | x <- [0..width-1], y <- [0..height-1]]

-- adds the supplied PlacedBlock to placedBlocks of the Board
placeBlock :: Board -> PlacedBlock -> Board
placeBlock (Board h w blocks) block = Board{boardHeight = h, boardWidth = w, placedBlocks = blocks ++ [block]}


isInBounds :: Board -> PlacedBlock -> Bool
isInBounds (Board h w _) (PlacedBlock (Block bh bw _) (Position x y)) = h > bh + y && w > bw + x && 0 <= bh + y && 0 <= bw + x


-- TODO: could probably be simplified by using occupiedPositions and listOccupiedpositions and checking if they contain at least one identical element

----------functions to check if a PlacedBlock collides with the PlacedBlocks already on a Board

-- checks if a new PlacedBlock can be placed on the board, or if it would collide with an already existing PlacedBlock on the Board
isColliding :: Board -> PlacedBlock -> Bool
isColliding board newBlock = do -- checks all positions the newBlock occupies
    let allPositions = occupiedPositions newBlock
    let checkPositionOnBoard = isOccupied board
    let positionsBlockedList = map checkPositionOnBoard allPositions
    any (==True) positionsBlockedList

-- checks if a Position is occupied on the Board
isOccupied :: Board -> Position -> Bool
isOccupied (Board _ _ placedBlocks) position = do
    let positionOccupied = occupies position
    let positionIsBlockedList = map positionOccupied placedBlocks -- checks all blocks, if they block this position
    any (==True) positionIsBlockedList

-- checks if the supplied Position is occupied by this Block
occupies :: Position -> PlacedBlock -> Bool
occupies (Position x y) (PlacedBlock (Block height width _) (Position blockX blockY)) = x < (blockX + width) && x >= blockX && y < (blockY + height) && y >= blockY

-- returns a List of all Positions, that a PlacedBlock occupies
occupiedPositions :: PlacedBlock -> [Position]
occupiedPositions (PlacedBlock (Block height width _) (Position x y)) = [Position {x=a, y=b} | a <- [x..x+width-1], b <- [y..y+height-1]]