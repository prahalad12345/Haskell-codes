
fib = 0:1:zipWith (+) fib (tail fib) 

main = do line<-getLine 
	  let n =read line::Int
	      list = take n fib 
	  --print list
	  --putStrLn $ unlines $ map show list
	  sequence $ map (putStrLn.show) list 
	  mapM_ (putStrLn.show) list
	  
{-
main = interact shortLinesOnly  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
-} 

--main = interact $ unlines . filter ((<10) . length) . lines 
