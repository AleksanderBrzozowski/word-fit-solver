import qualified WordFitSolver as Solver

main = do
    let m = Solver.matrix [
         "JULIETCARESS",
         "WEDDINGROMEO",
         "CHOCOLATERGF",
         "BOUQUETTESRI",
         "SWEETSLRUTOR",
         "PEVETUOONEOS",
         "BONNEDVNRAMT",
         "LRESAZOYGRTK",
         "RCIMEILSDRTI",
         "SIEDTQGUAORS",
         "CRNOEUUECWIS",
         "AEVGHNHEEKSD",
         "REEMBRACEETA",
         "DATINGSRSNAT",
         "SCOUPLEOYSNE",
         "CUPIDPRESENT"]
    let words = [
         "ADORER",
         "LYRE",
         "ARROW",
         "ODE",
         "BOUQUET",
         "POEM",
         "BRIDE",
         "PRESENT",
         "CARDS",
         "QUEEN",
         "CARESS",
         "RENDEZVOUS",
         "CHOCOLATE",
         "RING",
         "COUPLE",
         "ROMEO",
         "CUPID",
         "ROSES",
         "DATE",
         "RYE",
         "DATING",
         "SCENTS",
         "DEVOTION",
         "SENSE",
         "EMBRACE",
         "SONG",
         "FIRSTKISS",
         "SWEET",
         "GROOM",
         "TRISTAN",
         "HEART",
         "WEDDING",
         "HUGS",
         "ISEULT",
         "JULIET",
         "LOVE",
         "LUCK"  ]
    let combinations = Solver.combinations m
    let matchedPoints = concat $ map (\word -> Solver.matchedPoints combinations word) words
    putStrLn "Matrix:\n"
    Solver.printMatrix m
    putStrLn "\nSolution:\n"
    Solver.printSolution m matchedPoints
