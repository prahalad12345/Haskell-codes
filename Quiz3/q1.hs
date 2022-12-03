
main = do putStr "Enter a string:" 
          line <- getLine
          putStr "Enter a number:" 
          strnum <- getLine 
          let num = read strnum::Int 
          let result = resultgenerator line num 
          putStrLn ("Stuttered word:"++result)
          
resultgenerator xs y = [ i | i <- xs , j<- [1..y] ] 


