module Collision where

import GameElements


-- Solves the game. Returns all possible solutions for the supplied blocks and board.
-- This is the function you're going to want to call from the "frontend" to get all solutions to the game
solveGame :: [Block] -> Board -> [Board]
solveGame [] board = [board]
solveGame (currentBlock:remainingBlocks) board = concatMap (solveSingleBlock currentBlock) (solveGame remainingBlocks board)

-- Returns all possible solutions to legally (no overlapping, in bounds) place a single block on the board.
-- Algorithm: try to place the block on each square of the board -> create copy of board for each legal placement -> return these
solveSingleBlock :: Block -> Board -> [Board]
solveSingleBlock block board = placeOnPositionsIfLegal block board (allPositionsBoard board) []

-- Places the block on all of the supplied positions on the board where doing so is legal
placeOnPositionsIfLegal :: Block -> Board -> [Position] -> [Board] -> [Board]
placeOnPositionsIfLegal _ _ [] solutions = solutions
placeOnPositionsIfLegal block originalBoard (currentPosition:remainingPositions) alreadyFoundSolutions
    | illegalToPlace = updatedSolutions
    | otherwise = updatedSolutions ++ [placeBlock originalBoard placedBlock]
  where
    placedBlock = PlacedBlock { block = block, topLeftCorner = currentPosition }
    illegalToPlace = not (isInBounds originalBoard placedBlock) || isColliding originalBoard placedBlock
    updatedSolutions = placeOnPositionsIfLegal block originalBoard remainingPositions alreadyFoundSolutions

-- Adds the supplied PlacedBlock to placedBlocks of the Board.
placeBlock :: Board -> PlacedBlock -> Board
placeBlock board block = board{placedBlocks = placedBlocks board ++ [block]}

-- Checks if the supplied placedblock is fully within the bounds of the board.
isInBounds :: Board -> PlacedBlock -> Bool
isInBounds board (PlacedBlock (Block bh bw _) (Position x y)) = boardHeight board > (bh-1) + y && boardWidth board > (bw-1) + x && 0 <= (bh-1) + y && 0 <= (bw-1) + x  -- -1 on bh and bw, because for example a block on y=1 and bh=1 does not actually occupy space y=2

-- Returns true if the PlacedBlock would be placed on an already occupied space.
-- Checks if there are any shared Positions between the PlacedBlock to add and the ones already on the Board.
isColliding :: Board -> PlacedBlock -> Bool
isColliding board newBlock = anyEqualElements (allOccupiedPositionsPlacedBlock newBlock) (allOccupiedPositionsBoard board)

-- Returns true if the two lists contain at least one identical element.
anyEqualElements :: (Eq a) => [a] -> [a] -> Bool
anyEqualElements a b = any (`elem` b) a



-- Returns all Positions on the Board.
allPositionsBoard :: Board -> [Position]
allPositionsBoard (Board height width _) = [Position{x, y} | x <- [0..width-1], y <- [0..height-1]]

-- Returns all positions occupied by the placedBlocks of board.
allOccupiedPositionsBoard :: Board -> [Position]
allOccupiedPositionsBoard (Board _ _ placedBlocks) = concatMap allOccupiedPositionsPlacedBlock placedBlocks

-- Returns a List of all Positions occupied by the PlacedBlock.
allOccupiedPositionsPlacedBlock :: PlacedBlock -> [Position]
allOccupiedPositionsPlacedBlock (PlacedBlock (Block height width _) (Position x y)) = [Position {x=a, y=b} | a <- [x..x+width-1], b <- [y..y+height-1]]



-- old ----------------

-- checks if a new PlacedBlock can be placed on the board, or if it would collide with an already existing PlacedBlock on the Board
isCollidingOld :: Board -> PlacedBlock -> Bool
isCollidingOld board newBlock = do -- checks all positions the newBlock occupies
    let allPositions = allOccupiedPositionsPlacedBlock newBlock
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