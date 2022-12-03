combine [] = []
combine (x:xs) = (takeWhile (\y -> y==x) (x:xs) ):(combine $ (dropWhile (\y -> y==x) (x:xs) ))
