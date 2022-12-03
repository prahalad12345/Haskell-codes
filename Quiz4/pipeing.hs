import System.IO
import Text.Read
import Control.Monad

main = interact $ unlines . map calc . lines 

cal:: String -> String 
calc = show . eval (Just []) . mapM parse . words 

data Token = Plus | Minus | Mul | Div | Val Int deriving (Eq,Show)

parse t | "+" = Just Plus 
        | "-" = Just Minus
        | "*" = Just Mul 
        | "/" = Just Div 
        | otherwise = do x <- (readMaybe n ::Maybe Int) 
                         return (Val x) 
                         
      
type Stack = [Int]

eval :: Maybe Stack -> Maybe [Token] -> Maybe Stack 

eval mns mts = do ts <- mts 
                  ns <- mns 
                  if null ts then mns 
                             else eval (apply ns $ head ts ) (Just (tail ts))
                             
apply :: Stack -> Token -> Maybe Stack

apply :: Stack -> Token -> Maybe Stack
apply ns (Val n) = return (n:ns)
apply ns t       =   do   x   <- safeHead ns
                          y   <- safeTail ns >>= safeHead
                          ns' <- safeTail ns >>= safeTail
                          case t of 
                                Plus  -> return (y+x:ns')
                                Minus -> return (y-x:ns')
                                Mul   -> return (y*x:ns')
                                Div   -> do 
                                           z <- (y `safeDiv` x)
                                           return (z:ns')


safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead  _    = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail  _     = Nothing


safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
