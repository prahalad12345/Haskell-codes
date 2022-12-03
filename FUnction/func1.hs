double x=x+x 
quadruple x=double ( double x)
factorial n= product [1..n]
average ns = (sum ns ) `div` (length ns)

rotate [] _ = []
rotate xs 0 =xs 
rotate (x:xs) n | n `mod` (length xs) ==0   = xs | otherwise = rotate xs ((n-1) `mod` (length xs)) ++ [x]

nestedodd xs=[concat [x]| y <-xs,x<-y]
