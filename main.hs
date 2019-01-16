import qualified WordFitSolver as Solver
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Dynamic
import System.Environment

main = do
    args <- getArgs
    case args of
        [arg1, arg2] -> do
                            tableContent <- readFile arg1
                            let tableLines = lines tableContent
                            wordsContent <- readFile arg2
                            let words = lines wordsContent
                            let m = Solver.matrix tableLines

                            let combinations = Solver.combinations m
                            let matchedPoints = concat $ map (\word -> Solver.matchedPoints combinations word) words
                            putStrLn "Matrix:\n"
                            Solver.printMatrix m
                            putStrLn "\nSolution:\n"
                            Solver.printSolution m matchedPoints
        _ -> print "Usage: ./main tableFile wordsFile"