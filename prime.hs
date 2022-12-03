factors n = [i | i<-[1..n] , n `mod` i ==0]

checkprime n | factors n == [1,n] = "YES" | otherwise = "NO"

main = interact $ unlines .( map checkprime) . map read . lines 
