module DrawingTests where

import GameElements
import Drawing

---------tests for Board drawing related functions

--should draw 2x2 Board with 2x1 Block at (0,1) and 1x1 Block at (0,0)
testDraw :: IO()
testDraw = do
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Green}, topLeftCorner = Position {x=0, y=1}}
    let placedBlock2 = PlacedBlock { block = Block {blockHeight=1, blockWidth=1, color=Green}, topLeftCorner = Position {x=0, y=0}}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock1, placedBlock2]}
    draw board

testColorOf :: IO()
testColorOf = do
    let position1 = Position {x=0, y=1}
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Green}, topLeftCorner = position1}
    let colorOf1 = colorOf placedBlock1 == Green
    putStrLn ("Result of testColorOf - test 1: " ++ show colorOf1 ++ ", expected: True")
    
testColorAt :: IO()
testColorAt = do
    let position1 = Position {x=0, y=1}
    let placedBlock1 = PlacedBlock { block = Block {blockHeight=1, blockWidth=2, color=Green}, topLeftCorner = position1}
    let board = Board {boardHeight = 2, boardWidth = 2, placedBlocks = [placedBlock1]}
    let colorAt1 = colorAt position1 board == Just Green
    putStrLn ("Result of testColorAt - test 1: " ++ show colorAt1 ++ ", expected: True")
    let position2 = Position {x=0, y=0}
    let colorAt2 = colorAt position2 board == Nothing
    putStrLn ("Result of testColorAt - test 2: " ++ show colorAt2 ++ ", expected: True")