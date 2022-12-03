import Data.List

main :: IO()
main = do line<-getLine
          let n = read line::Int
              xs = fun n
          xss <- xs
          print(sum xss)



--fun :: Int -> [a]
fun n = magic n []

--magic :: Int -> [a] -> IO [a]
magic 0 xs = return  xs
magic n xs = do line <- getLine
                let x = read line::Int
                magic (n-1) (x:xs)
