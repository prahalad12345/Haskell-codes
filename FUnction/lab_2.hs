--flip [(a,b)]->[(b,a)]
--Eg [(1,'a'),(2,'b')] = [('a',1),('b',2)]

--list comprehension 
fliplist xs = [(b,a) | (a,b)<-xs]
--recursion 
fliprec []=[]
fliprec ((a,b):xs)= ((b,a):fliprec xs)

--dupli [a]->[a] duplicates each element in the list 
--Eg [1,2,2]->[1,1,2,2,2,2] 

--list compreshension 
duplilist xs = concat [[x,x] | x<-xs]
--concat perform concatenation of all the list in the list [[a]] -> [a]
duplilist2 xs= [i | i<-xs,j<-[1..2]]
--recursion 
duplirec []=[]
duplirec (x:xs) = (x:x:(duplirec xs))

--repli [a]->Int->[a] which is just replicating the n number of time  
--Eg-repli [1,2,3] 3 = [1,1,1,2,2,2,3,3,3]
--list comprehension
replilist xs n = [ i | i<-xs,j<-[1..n]]
--recursion 
replirec [] n = []
replirec (x:xs) n = [ x | i<-[1..n]] ++ replirec xs n

--filter (a->Bool)->[a]->[a]
--filter (6<) [1,2,3,4,5,6,7,7,7]
--[7,7,7] 
--list comprehension
filterlist func xs = [ i | i<-xs,func i ]
--recursion 
filterrec func [] = []
filterrec func (x:xs) | func x = (x:filterrec func xs) | otherwise = filterrec func xs

--take Int->[a]->[a] 
--list comprehension
minn a b | a>b = b | otherwise = b
takelist n xs = [xs!!i | i<-[0..((minn (length xs) n)-1)]]
--recursion 
takerec _ [] = []
takerec 0 xs = []
takerec n (x:xs) = (x:takerec (n-1) xs)

--implement mergesort 
merge [] ys = ys 
merge xs [] = xs
merge (x:xs) (y:ys) | (x>y) = (y:merge (x:xs) ys) | otherwise = (x:merge xs (y:ys))
--mergesort 
mergesort [] = [] 
mergesort (x:[])=(x:[])
mergesort xs = merge  (mergesort (take ((length xs) `quot` 2) xs))   (mergesort  (drop ((length xs) `quot` 2) xs ))


--vowels extractws words from the list "abcde" = "ae"
--list comprehension

vowellist xs = [x | x<-xs , x=='a' || x=='e' || x=='i' || x=='o' || x=='u']
vowelrec [] = []
--recursion
vowelrec (x:xs) | x=='a' || x=='e' || x=='i' || x=='o' || x=='u' = (x:vowelrec xs) | otherwise = vowelrec xs


--doubleeven - doubles all even number and leaves the odd number = [2,3,4,5] = [4,3,8,5] 
--list comprehension 

doubleevenlist xs= [ if x `mod` 2 ==0 then 2*x else x | x<-xs]


--recursive 

doubleevenrec [] = [] 
doubleevenrec (x:xs) | x `mod` 2==0 = (2*x:doubleevenrec xs) | otherwise = (x:doubleevenrec xs)

--elem return true if element exist in the list otherwise return false
--list comprehension
elemlist xs n= or[True|x<-xs,x==n]
--recursive 
elemrec [] n= False 
elemrec (x:xs) n | x==n = True | otherwise = elemrec xs n


--riffle takes two list as input and adds them alternatively  [1,2,3] [4,5,6] = [1,4,2,5,3,6]
--listcomprehension
rifflelist xs ys =concat [[x,y] | (x,y) <- zip xs ys] 
--recursion 
rifflerec [] [] = []
rifflerec (x:xs) (y:ys) = (x:y:rifflerec xs ys) 


--zipwith takes an operation two list and performs operation on the two list http://zvon.org/other/haskell/Outputprelude/zipWith_f.html
--listcomprehension 
zipwithlist fun xs ys = [fun a b | (a,b) <- zip xs ys]
--recursion 
zipwithrec fun [] [] = []
zipwithrec fun xs [] = []
zipwithrec fun  [] ys = []
zipwithrec fun (x:xs) (y:ys) = (fun x y :  zipwithrec fun xs ys ) 
