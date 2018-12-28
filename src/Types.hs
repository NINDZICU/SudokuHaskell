module Types where

type Coordinate = (Int, Int)

gameSize :: Int
gameSize = 9

data Cell = Cell {
                   visible :: Bool
                 , value :: Int
                 } deriving Show

-- Setter for visible
setVisible :: Cell -> Bool -> Cell
setVisible cell visible = cell {
                                visible = visible
                               }

-- Setter for value
setValue :: Cell -> Int -> Cell
setValue cell value = cell {
                            value = value
                           }