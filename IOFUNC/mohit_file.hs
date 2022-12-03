import System.IO
import Text.Read
import Control.Monad

data Token = Zero | One | Nothingg deriving Show


main::IO()
main = do line<-getLine
          let listOfToken = mapM charToToken line
              ans = binToInt listOfToken
          putStrLn (show ans)
          main

charToToken :: Char -> Maybe Token
charToToken c | c=='1'     = One
              | c=='0'     = Zero
              | otherwise   = Nothing




anss :: [Token] -> Maybe Int
anss [Nothingg] = Nothing
-- try it with foldl as well
anss xs = Just ( sum [ (magic((reverse xs)!!(i)))*(2^i) | i<-[0..((length xs) - 1)] ] )
         where magic n = if n == One then 1 else 0







{-
data Token = One | Zero deriving (Show, Eq)
charToToken :: Char -> Token
charToToken = \t -> case t of
                        '1'->One
                        '0'->Zero



nttList::String->[Token]
nttList xs = [  charToToken(xs!!i) |i<-[0..(length xs - 1)] ]




help sum [] index = sum
help sum (x:xs) index = help (sum + y*(2^index)) xs (index+1) where 
                                                                  y = if x=='1' then 1 else 0






main = do line<-getLine
          let listoftoken = nttList line
          let ans = show listoftoken
          print ans
          main
-}




{-
check :: String -> Bool
check xs =  and [ ( xs!!i=='0' || xs!!i=='1' )  | i<-[0..(length xs - 1)] ]



help sum [] index = sum
help sum (x:xs) index = help (sum + y*(2^index)) xs (index+1) where 
                                                                  y = if x=='1' then 1 else 0



binToInt::String->Int
binToInt xs = help 0 (reverse xs) 0



main :: IO()
main = do line<-getLine
          let isValid = check line
          if not isValid then do putStrLn "Nothing"
          else do
                 let ans = binToInt line
                 print ans
          main
        
-}
