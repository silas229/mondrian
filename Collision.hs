module Collision where

import GameElements


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