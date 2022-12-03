combinehelper prev [] = ([]:[]) 
combinehelper prev (x:xs) | x==prev = [x]++(combine x xs) | otherwise = [] : combine x (x:xs) 

combine [] = [] 
combine (x:xs) = combinehelper x (x:xs)
