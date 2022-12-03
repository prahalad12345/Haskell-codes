main::IO()
main = do line<-getLine
          let listOfToken = mapM charToToken line
              ans = listOfToken >>= fun
          putStrLn (show ans)
          main



-- By deriving(Show, Eq) => Token will also be considered in the class....
-- ...Show and Token. So that you can easily use functions like print, ==, /=, etc on them.
data Token = Zero | One deriving(Show, Eq)


charToToken :: Char -> Maybe Token

charToToken c | c=='1'     = Just One
              | c=='0'     = Just Zero
              | otherwise  = Nothing
              
 
magic n = if n==One then 1 else 0
fun :: [Token] -> Maybe Int
fun xs = Just(sum [    magic(ys!!i)*(2^i)      |i<-[0..n]] )
        where ys = reverse xs
              n = length xs - 1
