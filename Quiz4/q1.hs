main = do num <- getLine 
          print(result num)
          main


result xs | checker xs = Just(binary xs)
          | otherwise = Nothing
    


binary xs = calculator 1 xs  

checker xs = and[False|x<-xs,x/='0' && x/='1'] 


calculator _ [] = 0

calculator fac (xs) | last xs == '0' = calculator (fac*2) (take (length xs -1) xs ) 
                    | last xs == '1' = fac + calculator (fac*2) (take (length xs -1) xs ) 
                    | otherwise =  0

