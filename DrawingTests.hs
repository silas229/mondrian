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