import System.IO
import Text.Read
import Control.Monad

data Binn = One | Zero  deriving (Show,Eq) 

main = interact $ unlines . map bintoint . lines 

bintoint = show . calc . mapM parse

parse :: Char -> Maybe Binn

parse t | t=='1' = Just One 
        | t=='0' = Just Zero 
        |otherwise = Nothing

calc :: Maybe [Binn] -> Maybe Int  
calc xs = do x <- xs 
             if null x then return  0 
                      else do y <- safeHead x
                              z <- (calc (safeTail x))
                              return ((converter y) *(2^((length x) -1)) + z )
                              where converter n = if n == One then 1 else 0
             
             


safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead  _    = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail  _     = Nothing 
             
