getline=do x <- getChar 
           if x == '\n' then 
              return [] 
           else 
              do xs <- getline 
                 return (x:xs)
                 
putstr [] = return () 
putstr (x:xs) = do putChar x
                   putstr xs
                   
putstrln [] = return () 
putstrln (x:xs) = do putChar x
                     putstrln xs
                     
len = do putstr "Enter a string:" 
         xs <- getline 
         putstr "length of string:"  
         putstr (show(length xs))             
