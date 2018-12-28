module Matrix (
    Matrix,
    mapRows,
    prettyPrint,
    Cell( .. )
) where

type Matrix = [[Char]]
type MapMatrix = Matrix -> ([Char] -> [Char]) -> Matrix
data Cell = Cell Int Int deriving Show
type Diagonal = Diagonal Cell Cell

mapRows :: MapMatrix
mapRows m f = map f m

-- diagonals :: Matrix -> [Diagonal]
-- diagonals m = 
--     where
--         diagonals :: Matrix -> Int -> Int
--         diagonals m nrows ncols  

diagonalCells :: Matrix -> Cell -> Cell -> [Cell]
diagonalCells m (Cell fromRow fromColumn) (Cell toRow toColumn) = cells m (Cell fromRow fromColumn) (Cell toRow toColumn) (direction fromRow toRow)
    where
        direction :: Int -> Int -> Int
        direction fromRow toRow | fromRow > toRow = -1
                                | fromRow == toRow = 0
                                | fromRow < toRow = 1
        cells :: Matrix -> Cell -> Cell -> Int -> [Cell]
        cells m (Cell fromRow fromColumn) (Cell toRow toColumn) d
            | fromRow == toRow = [Cell fromRow fromColumn]
            | otherwise = [Cell fromRow fromColumn] ++ cells m (Cell (fromRow + d) (fromColumn +d)) (Cell toRow toColumn) d

ncols :: Matrix -> Int
ncols m = length $ m !! 0

at :: Matrix -> Int -> Int-> String
at m row col = [(m !! row) !! col]

replaceNth :: Int -> Char -> [Char] -> [Char]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

prettyPrint :: Matrix -> IO ()
prettyPrint m = putStr $ unlines m
