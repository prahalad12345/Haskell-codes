import Control.Monad  
 -- sequence ( map xxx) = mapM_
main = do putStrLn "Enter the number?"
          line<-getLine 
          let n = read line::Int 
          lis <- forM [1..n] (\a -> do x <-getLine 
                                       let val = read x::Int 
                                       return val)
          print (sum lis)
{-
ans 0 = 0
ans n = do line<-getLine 
           let nn = read line::Int 
           return (nn + ans (n-1))

-}     

head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
