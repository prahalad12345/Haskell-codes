import System.IO 
import Text.Read 
import Control.Monad 

safediv _ 0 = Nothing 
safediv  x y = Just (x `div` y) 

safeTail (_:xs) = Just xs 
safeTail _ = Nothing 

safeInput x = readMaybe x:: Maybe Int
