fac 0 = 1
fac n= n* fac (n-1)

fib 0 = 0 
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

len [] = 0
len (_:xs) = 1 + len xs

su [] = 0
su (x:xs) = su xs + x

[] !!! n = 0
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

[] +++ xs = xs 
(x:xs) +++ ys = x:(xs +++ ys)

productt [] = 1
productt (x:xs) = x * product xs

insert x [] = [x]
insert x (a:xs) | x>a = [a] ++ insert x xs | otherwise = [x] ++ (a:xs) 

insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)

zipp [] [] = []
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys)= [(x,y)] ++ zipp xs ys

dropp _ [] = []
dropp 0 xs = xs 
dropp x (a:xs)= dropp (x-1) xs

reversee [] = []
reversee (x:xs) = reversee xs ++ [x]

anotherreverese [] ys = ys 
anotherreverese (x:xs) ys = anotherreverese xs (x:ys)

--the above function is used to reverse in this we transfer value from 1 list to another list 
quicksort []=[]
quicksort (x:xs) = quicksort a ++ [x] ++ quicksort b 
 where 
  a=[i | i<-xs, i<=x ] 
  b=[i | i<-xs , i>x ]
  
  
 --determnine if a number is even or odd using recursion 
 
evenn 0 = True 
evenn n = oddd (n-1) 

oddd 0 = False 
oddd n = evenn (n-1)


--init
initt [] = [] 
initt [x] = [] 
initt (x:xs) = (x:initt xs)

--last 
lastt [x] = x
lastt (x:xs) = lastt xs 

--map 
--recursion
mapp f [] = [] 
mapp f (x:xs) = (f x : map f xs)
--list comprehension 
maplist f xs = [ f x | x<-xs ] 



