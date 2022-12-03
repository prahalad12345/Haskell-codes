import System.IO
import Text.Read
import Control.Monad


data Tokens = I | D deriving (Show , Eq)
 
main = interact $ unlines . map counter . lines 

counter :: [Char] -> String
counter = show . calc . mapM parse .reverse

parse :: Char -> Maybe Tokens

parse t | t=='i' = Just I 
        | t=='d' = Just D
        |otherwise = Nothing
 

parseforprefix :: Int -> Maybe Int
parseforprefix t | t >= 0  = Just t 
                 | otherwise = Nothing      


calc::Maybe [Tokens] -> Maybe Int
calc xs = do x <- xs 

             if null x then return 0 
  
             else do y <- safeHead x 
                     z <- calc $ safeTail x 
                     parseforprefix ((converter y) + z)
         
             where converter xx = if xx == I then 1 else -1 
                

                
--Just safehead
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead  _    = Nothing

--Just safetail
safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail  _     = Nothing 
