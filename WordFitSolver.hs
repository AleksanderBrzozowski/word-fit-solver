module WordFitSolver (
    Point ( .. ),
    Cell ( .. ),
    matrix,
    combinations,
    printSolution,
    matchedPoints,
    printMatrix,
) where

import Data.List

type Point = (Int, Int)
type Cell = (Char, Point)
type Matrix = [[Cell]]

matrix :: [[Char]] -> Matrix
matrix rows = zipWith (\row rowIndex -> matrixCols row rowIndex) rows [0..]
    where
        matrixCols :: [Char] -> Int -> [Cell]
        matrixCols row rowIndex = zipWith (\letter colIndex -> (letter, (rowIndex, colIndex))) row [0..]

combinations :: Matrix -> [Cell]
combinations m = rows m ++ cols m ++ diagonals m

rows :: Matrix -> [Cell]
rows m = concat m

cols :: Matrix -> [Cell]
cols ([]:_) = []
cols m = map head m ++ cols (map tail m)

diagonals :: Matrix -> [Cell]
diagonals m = diagonals' m ++ diagonals' (invert m)
    where
        invert :: Matrix -> Matrix
        invert []     = []
        invert (x:xs) = (invert xs) ++ [x]
        diagonals' :: Matrix -> [Cell]
        diagonals' m = main m ++ downTriangle m ++ upTriangle m
            where
                main :: Matrix -> [Cell]
                main [] = []
                main ([]:_) = []
                main (x:xs)  = head x : main (map tail xs)
                downTriangle :: Matrix -> [Cell]
                downTriangle [] = []
                downTriangle (r:rs) = main rs ++ downTriangle rs
                upTriangle :: Matrix -> [Cell]
                upTriangle m = upTriangle' (map tail m)
                    where
                        upTriangle' :: Matrix -> [Cell]
                        upTriangle' m | length (head m) == 1 = head m
                                      | otherwise = main m ++ upTriangle' (map tail m)

matchedPoints :: [Cell] -> [Char] -> [Point]
matchedPoints [] _ = []
matchedPoints (w:ws) pattern = matchedPoints' (w:ws) pattern [] ++ (matchedPoints ws pattern)
    where
        matchedPoints' :: [Cell] -> [Char] -> [Point] -> [Point]
        matchedPoints' _ [] points = points
        matchedPoints' [] _ points = []
        matchedPoints' ((c, point):ws) (p:ps) points | c == p = matchedPoints' ws ps (points ++ [point])
                                                     | otherwise = [] 


printSolution :: Matrix -> [Point] -> IO()
printSolution m excludedPoints = mapM_ (\x -> putStrLn $ row x excludedPoints) m
    where
        row :: [Cell] -> [Point] -> [Char]
        row rowCells excludedPoints = map (\(c,p) -> if (elem p excludedPoints) then ' ' else c) rowCells

printMatrix :: Matrix -> IO()
printMatrix m = mapM_ (\x -> putStrLn (map(\(c, _) -> c) x)) m
    