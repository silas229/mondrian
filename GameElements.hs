module GameElements where

--------data type definions

data Color = Red | Green | Blue deriving (Eq, Show)

data Position = Position
    {   x :: Int,
        y :: Int
    } deriving Eq

data Block = Block -- a block that has not been placed on the board yet -> has no position
    {   blockHeight :: Int
    ,   blockWidth  :: Int
    ,   color :: Color
    } deriving Eq

instance Ord Block where
    compare :: Block -> Block -> Ordering
    compare block1 block2 =
        compare (blockHeight block1 * blockWidth block1) (blockHeight block2 * blockWidth block2)



data PlacedBlock = PlacedBlock -- a block that has been placed on the board -> has a position
    {   block :: Block
    ,   topLeftCorner :: Position
    }

data Board = Board
    {   boardHeight :: Int
    ,   boardWidth :: Int
    ,   placedBlocks :: [PlacedBlock]
    }
