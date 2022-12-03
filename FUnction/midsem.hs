fac 0 = 1
fac n = n * fac (n-1) 

prodlis [] = 1
prodlis (x:xs) = x * prodlis xs

lengthg [] =0
lengthg (_:xs) = 1 + lengthg xs

insert x [] = [x]
insert x (y:xs) | x>y = [y] ++ insert x xs | otherwise = (x:y:xs) 

isort [] = []
isort (x:xs) = insert x (isort xs)


qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
 where 
  smaller = [i | i<-xs , i<=x ] 
  larger = [i | i<-xs , i>x]
  
sumdown 0 = 0 
sumdown n = n + sumdown (n-1)

euclid x 0 = x
euclid 0 y = y
euclid x y | x>y = euclid (x `mod` y) y | otherwise = euclid x  (y `mod` x)

(x:xs)!!!0 = x 
[]!!!_ = -1 
(x:xs)!!!n = xs!!!(n-1) 

initt (x:[]) = []
initt (x:xs) = (x:initt xs)

concatt [] = []
concatt (x:xs) = x ++ concat xs

andd [] = True 
andd (x:xs) | x = and xs | otherwise = False

elemm _ [] = False 
elemm y (x:xs) | x==y = True | otherwise = elemm y xs

merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x>y = (y:merge (x:xs) ys)  | otherwise = (x:merge xs (y:ys))

msort [] = [] 
msort (x:[]) = (x:[])
msort xs = merge (msort (take ((length xs) `quot` 2) xs ))  (msort (drop ((length xs) `quot` 2) xs ))

unfold p h t x | p x = [] | otherwise = (h x : unfold p h t (t x) )

safetail [] = []
safetail (x:xs) = xs
