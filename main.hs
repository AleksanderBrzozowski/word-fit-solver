import WordFitSolver

main = do
    let m = matrix(["ABC","DEF","GHI"])
    let combination = combinations m
    printSolution m [(Cell 'A' (Point 0 0))]
