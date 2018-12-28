module WordFitSolver (
    Point ( .. ),
    Cell ( .. ),
    matrix,
    combinations,
    printSolution,
) where

import Data.List

data Point = Point Int Int deriving (Eq, Show)
data Cell = Cell Char Point deriving (Eq, Show)
type Matrix = [[Cell]]

matrix :: [[Char]] -> Matrix
matrix rows = zipWith (\row rowIndex -> matrixCols row rowIndex) rows [0..]
    where
        matrixCols :: [Char] -> Int -> [Cell]
        matrixCols row rowIndex = zipWith (\letter colIndex -> Cell letter (Point rowIndex colIndex)) row [0..]

combinations :: Matrix -> [[Cell]]
combinations m = rows m
    where
        rows :: Matrix -> [[Cell]]
        rows m = m

printSolution :: Matrix -> [Cell] -> IO()
printSolution m excludedCells = mapM_ (\x -> putStrLn $ row x excludedCells) m
    where
        row :: [Cell] -> [Cell] -> [Char]
        row rowCells excludedCells = map (\c -> if (elem c excludedCells) then ' ' else cellLetter c) rowCells
        cellLetter :: Cell -> Char
        cellLetter (Cell c p) = c

-- printMatrix :: Matrix -> IO()
-- printMatrix m = mapM_ (\x -> putStrLn $ row x excludedCells) m
    