import Data.Char

main = do putStrLn "What you name?"
          line <- getLine
          putStrLn ("Hey Line "++line) 
          let upperline = map toUpper line 
          putStrLn ("Your crazy name is "++ upperline )
       
