module SolvingTests where

import GameElements
import Solving
import Drawing


-- automated tests for various functions in module Solving

main :: IO()
main = do
    putStrLn "Running tests for game solving and collision related functions"
    testSolveGame1Block
    testSolveGame2Blocks
    testSolveGame2BigBlocks
    testIsInBounds
    testPlaceOnPositionsIfPossible
    testPlaceBlock
    testIsColliding
    testIsCollidingOld
    testOccupies
    testIsOccupied
    putStrLn "Finished tests for game solving and collision related functions"

------tests for collisiondetection related functions

testSolveGame1Block :: IO()
testSolveGame1Block = do
    let placedBlock = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
    let block = Block {blockHeight=1, blockWidth=1, color=Red}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock]}
    putStrLn ("expected 3, actual: " ++ (show (length (solveGame [block] board))))
    draw (solveGame [block] board !! 1)

testSolveGame2Blocks :: IO()
testSolveGame2Blocks = do
    let placedBlock = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=0}}
    let block1 = Block {blockHeight=1, blockWidth=1, color=Red}
    let block2 = Block {blockHeight=1, blockWidth=1, color=Black}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock]}
    putStrLn ("expected 6, actual: " ++ show (length (solveGame [block1, block2] board)))
    draw (solveGame [block1, block2] board !! 1)

testSolveGame2BigBlocks :: IO()
testSolveGame2BigBlocks = do
    let block1 = Block {blockHeight=2, blockWidth=1, color=Red}
    let block2 = Block {blockHeight=2, blockWidth=1, color=White}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = []}
    putStrLn ("expected 4, actual: " ++ show (length (solveGame [block1, block2] board)))
    draw (solveGame [block1, block2] board !! 1)

testSolveGameNoSolution :: IO()
testSolveGameNoSolution = do
    let block1 = Block {blockHeight=3, blockWidth=1, color=Red}
    let block2 = Block {blockHeight=2, blockWidth=1, color=White}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = []}
    putStrLn ("expected 0, actual: " ++ show (length (solveGame [block1, block2] board)))

testIsInBounds :: IO()
testIsInBounds= do
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=1}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = []}
    let inBounds1 = isInBounds board placedBlock1
    putStrLn ("Result of tesIsInBounds - test 1: " ++ show inBounds1 ++ ", expected: False")
    let placedBlock2 = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
    let inBounds2 = isInBounds board placedBlock2
    putStrLn ("Result of tesIsInBounds - test 2: " ++ show inBounds2 ++ ", expected: True")

testPlaceOnPositionsIfPossible :: IO()
testPlaceOnPositionsIfPossible = do
    let block = Block {blockHeight=1, blockWidth=1, color=Red}
    let placedBlock = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock]}
    let positions = [Position {x=0, y=0}, Position {x=0, y=1}]
    let results = placeOnPositionsIfLegal block board positions []
    putStrLn (show (length results))

testPlaceBlock :: IO()
testPlaceBlock = do
    let block = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = []}
    let (Board _ _ placedBlocks) = placeBlock board block
    putStrLn (show (length placedBlocks))

testIsColliding :: IO()
testIsColliding = do -- 2x2 Board, 1x2 Block at (0,0). Tests placing a 1x2 Block at (1,0), a 2x1 Block at (0,1), and a 1x1 Blocks at (0,0), (1,0), (0,1) and (1,1).
    let placedBlock = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock]}
    let newBlock1 = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=Red}, topLeftCorner = Position {x=1, y=0}}
    let isCollidingResult1 = isColliding board newBlock1
    putStrLn ("Result of testColliding - test 1: " ++ show isCollidingResult1 ++ ", expected: False")
    let newBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Blue}, topLeftCorner = Position {x=0, y=1}}
    let isCollidingResult2 = isColliding board newBlock2
    putStrLn ("Result of testColliding - test 2: " ++ show isCollidingResult2 ++ ", expected: True")
    let newBlock3 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=0}}
    let isCollidingResult3 = isColliding board newBlock3
    putStrLn ("Result of testColliding - test 3: " ++ show isCollidingResult3 ++ ", expected: True")
    let newBlock4 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=1, y=0}}
    let isCollidingResult4 = isColliding board newBlock4
    putStrLn ("Result of testColliding - test 4: " ++ show isCollidingResult4 ++ ", expected: False")
    let newBlock5 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=1}}
    let isCollidingResult5 = isColliding board newBlock5
    putStrLn ("Result of testColliding - test 5: " ++ show isCollidingResult5 ++ ", expected: True")
    let newBlock6 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=1, y=1}}
    let isCollidingResult6 = isColliding board newBlock6
    putStrLn ("Result of testColliding - test 5: " ++ show isCollidingResult6 ++ ", expected: False")

testIsCollidingOld :: IO()
testIsCollidingOld = do -- 2x2 Board, 1x2 Block at (0,0). Tests placing a 1x2 Block at (1,0), a 2x1 Block at (0,1), and a 1x1 Blocks at (0,0), (1,0), (0,1) and (1,1).
    let placedBlock = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock]}
    let newBlock1 = PlacedBlock { block = Block {blockHeight=2, blockWidth=1, color=Red}, topLeftCorner = Position {x=1, y=0}}
    let isCollidingResult1 = isCollidingOld board newBlock1
    putStrLn ("Result of testColliding - test 1: " ++ show isCollidingResult1 ++ ", expected: False")
    let newBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Blue}, topLeftCorner = Position {x=0, y=1}}
    let isCollidingResult2 = isCollidingOld board newBlock2
    putStrLn ("Result of testColliding - test 2: " ++ show isCollidingResult2 ++ ", expected: True")
    let newBlock3 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=0}}
    let isCollidingResult3 = isCollidingOld board newBlock3
    putStrLn ("Result of testColliding - test 3: " ++ show isCollidingResult3 ++ ", expected: True")
    let newBlock4 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=1, y=0}}
    let isCollidingResult4 = isCollidingOld board newBlock4
    putStrLn ("Result of testColliding - test 4: " ++ show isCollidingResult4 ++ ", expected: False")
    let newBlock5 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=0, y=1}}
    let isCollidingResult5 = isCollidingOld board newBlock5
    putStrLn ("Result of testColliding - test 5: " ++ show isCollidingResult5 ++ ", expected: True")
    let newBlock6 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Blue}, topLeftCorner = Position {x=1, y=1}}
    let isCollidingResult6 = isCollidingOld board newBlock6
    putStrLn ("Result of testColliding - test 5: " ++ show isCollidingResult6 ++ ", expected: False")

testOccupies :: IO()
testOccupies = do -- 2x1 Block at (0,1), testing Positions (0,0) and (1,1)
    let placedBlock = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=White}, topLeftCorner = Position {x=0, y=1}}
    let position1 = Position {x=0, y=0}
    let occupies1 = occupies position1 placedBlock
    putStrLn ("Result of testOccupies - test 1: " ++ show occupies1 ++ ", expected: False")
    let position2 = Position {x=1, y=1}
    let occupies2 = occupies position2 placedBlock
    putStrLn ("Result of testOccupies - test 2: " ++ show occupies2 ++ ", expected: True")

testIsOccupied :: IO()
testIsOccupied = do -- 2x2 Board with 2x1 Block at (0,1) and 1x1 Block at (0,0), testing Positions (0,0), (1,1) and (1,0)
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=White}, topLeftCorner = Position {x=0, y=1}}
    let placedBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=White}, topLeftCorner = Position {x=0, y=0}}
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