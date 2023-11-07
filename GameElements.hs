module GameElements where

--------data type definions

data Color = Red | Green | Blue deriving (Eq, Show)

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
