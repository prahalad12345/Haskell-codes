import System.IO 

--have to debug

data Token = Plus | Minus | Mul | Div | Val Int deriving (Show,Eq)
type Stack = [Int]

main = interact $ unlines . map calc . lines 

calc::String -> String 
calc = show . eval [] . map parse . words

parse = \t -> case t of 
                   "+" -> Plus 
                   "-" -> Minus 
                   "*" -> Mul 
                   "/" -> Div
                   n -> Val (read n::Int)


eval ns [] = ns 
eval ns (t:ts) = eval (apply ns t ) ts 

apply ns (Val n) = (n:ns)
apply ns t = let x = head ns 
                 y = head $ tail ns 
                 ns' = drop 2 ns 
              in
              case t of 
                   Plus -> (x+y):ns'
                   Minus -> (y-x):ns' 
                   Div -> (y `div` x):ns' 
                   Mul -> (y*x):ns'
