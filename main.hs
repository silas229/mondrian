
------tests for collisiondetection related functions

testIsColliding :: IO()
testIsColliding = do -- 2x2 Board, 1x2 Block at (0,0). Tests placing a 1x2 Block at (1,0), a 2x1 Block at (0,1), and a 1x1 Blocks at (0,0), (1,0), (0,1) and (1,1).
    let placedBlock = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=Green}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock]}
    let newBlock1 = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=Red}, topLeftCorner = Position {x=1, y=0}}
    let isColliding1 = isColliding board newBlock1
    putStrLn ("Result of testColliding - test 1: " ++ show isColliding1 ++ ", expected: False")
    let newBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Blue}, topLeftCorner = Position {x=0, y=1}}
    let isColliding2 = isColliding board newBlock2
    putStrLn ("Result of testColliding - test 2: " ++ show isColliding2 ++ ", expected: True")
    let newBlock3 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=0}}
    let isColliding3 = isColliding board newBlock3
    putStrLn ("Result of testColliding - test 3: " ++ show isColliding3 ++ ", expected: True")
    let newBlock4 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=1, y=0}}
    let isColliding4 = isColliding board newBlock4
    putStrLn ("Result of testColliding - test 4: " ++ show isColliding4 ++ ", expected: False")
    let newBlock5 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=1}}
    let isColliding5 = isColliding board newBlock5
    putStrLn ("Result of testColliding - test 5: " ++ show isColliding5 ++ ", expected: True")
    let newBlock6 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=1, y=1}}
    let isColliding6 = isColliding board newBlock6
    putStrLn ("Result of testColliding - test 5: " ++ show isColliding6 ++ ", expected: False")

testOccupies :: IO()
testOccupies = do -- 2x1 Block at (0,1), testing Positions (0,0) and (1,1)
    let placedBlock = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Green}, topLeftCorner = Position {x=0, y=1}}
    let position1 = Position {x=0, y=0}
    let occupies1 = occupies position1 placedBlock
    putStrLn ("Result of testOccupies - test 1: " ++ show occupies1 ++ ", expected: False")
    let position2 = Position {x=1, y=1}
    let occupies2 = occupies position2 placedBlock
    putStrLn ("Result of testOccupies - test 2: " ++ show occupies2 ++ ", expected: True")

testIsOccupied :: IO()
testIsOccupied = do -- 2x2 Board with 2x1 Block at (0,1) and 1x1 Block at (0,0), testing Positions (0,0), (1,1) and (1,0)
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Green}, topLeftCorner = Position {x=0, y=1}}
    let placedBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Green}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock1, placedBlock2]}
    let position1 = Position {x=0, y=0}
    let isOccupied1 = isOccupied board position1
    putStrLn ("Result of testIsOccupied - test 1: " ++ show isOccupied1 ++ ", expected: True")
    let position2 = Position {x=1, y=1}
    let isOccupied2 = isOccupied board position2
    putStrLn ("Result of testIsOccupied - test 2: " ++ show isOccupied2 ++ ", expected: True")
    let position3 = Position {x=1, y=0}
    let isOccupied3 = isOccupied board position3
    putStrLn ("Result of testIsOccupied - test 3: " ++ show isOccupied3 ++ ", expected: False")

---------tests for Board drawing related functions

--should draw 2x2 Board with 2x1 Block at (0,1) and 1x1 Block at (0,0)
testDraw :: IO()
testDraw = do
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Green}, topLeftCorner = Position {x=0, y=1}}
    let placedBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Green}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock1, placedBlock2]}
    draw board

--------data type definions

data Color = Red | Green | Blue

data Position = Position
    {   x :: Int,
        y :: Int
    }

data Block = Block -- a block that has not been placed on the board yet -> has no position
    {   blockHeight :: Int
    ,   blockWidth  :: Int
    ,   color :: Color
    }

data PlacedBlock = PlacedBlock -- a block that has been placed on the board -> has a position
    {   block :: Block
    ,   topLeftCorner :: Position
    }

data Board = Board
    {   boardHeight :: Int
    ,   boardWidth :: Int
    ,   placedBlocks :: [PlacedBlock]
    }


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
multiInsertAt toInsert original indices =
    if indices == []
        then original
        else multiInsertAt toInsert (insertAt toInsert original (head indices)) (tail indices)

-- replaces the element in the original list at the supplied index with toInsert
replaceAt :: [a] -> [a] -> Int -> [a]
replaceAt toInsert original index = do
    let( start , _:end ) = splitAt index original
    start ++ toInsert ++ end

-- uses replaceAt to recursively replace the elements in the original List at all supplied indices with toInsert
multiReplaceAt :: [a] -> [a] -> [Int] -> [a]
multiReplaceAt toInsert original indices =
    if indices == []
        then original
        else multiReplaceAt toInsert (replaceAt toInsert original (head indices)) (tail indices)


----------functions to check if a PlacedBlock collides with the PlacedBlocks already on a Board
-- TODO: could probably be simplified by using occupiedPositions and listOccupiedpositions and checking if they contain at least one identical element

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