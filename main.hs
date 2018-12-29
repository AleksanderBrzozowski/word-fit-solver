import WordFitSolver

main = do
    let m = matrix [
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
    let matrixCombinations = combinations m
    let cells = concat (map (\word -> matchedPoints matrixCombinations word) words)
    putStrLn "Matrix:\n"
    printMatrix m
    putStrLn "\nSolution:\n"
    printSolution m cells
