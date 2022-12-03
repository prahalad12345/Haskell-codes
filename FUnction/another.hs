n=a `div` length xs
 where
  a=10
  xs=[1,2,3,4,5]
  
safehead [] = Nothing 
safehead (x:xs) = Just x

safediv _ 0 = Nothing 
safediv n m = Just (n `div` m)

data List a = Empty | Cons a (List a) deriving Show--,Eq,Read 
--used to deriv the funcs

head' :: List a -> a
head' (Cons x xs) = x

len Empty = 0 
len ( Cons _ xs ) = 1 + len xs

data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

search x (Nil) = False 

search x (Node ltree a rtree ) = (x==a) || (search x ltree) || (search x rtree) 


insert x (Nil) = Node Nil x Nil 
insert x (Node ltree a rtree) | x<=a = ( Node (insert x ltree) a (rtree) ) | otherwise = ( Node (ltree) a (insert x rtree) )
{-
--find inorder successor
succ (Node Nil x _) = x 
succ (Node ltree _ _) = succ ltree 

--delete

delete x Nil = Nil 
delete x (Node Nil a Nil) | x==a = Nil | otherwise = (Node Nil a Nil)
delete x (Node Nil a rtree ) | x==a = rtree | x>a = (Node Nil a (delete x rtree)) | otherwise = (Node Nil a rtree)
delete x (Node ltree a Nil ) | x==a = ltree | x<a = (Node (delete x ltree) a Nil ) | otherwise =  (Node ltree a Nil )
delete x (Node ltree a rtree ) | x==a = Node ltree (succ rtree) (delete (succ rtree) rtree ) | x<y = Node (delete x ltree) a rtree | otherwise = Node ltree a (delete x rtree)  
-}

data Treee a = Leaf a | Nodee (Treee a) (Treee a) 
leafcount (Leaf _) = 1 
leafcount (Nodee x y ) = leafcount x + leafcount y 

balanced (Nodee x y) | (leafcount x - leafcount y)<=1 && (leafcount x - leafcount y)>=(-1) = True | otherwise = False


yield (Leaf x) = [x] 
yield (Nodee x y) = yield x ++ yield y

foldtree f v Nil = v 
foldtree f v (Node ltree a rtree ) = f a (foldtree f v ltree) (foldtree f v rtree)

height = foldtree (\x l r -> 1 + max l r) 0


buildtree [] = Nil 
buildtree (x:[]) =  Node Nil x Nil 
buildtree xs | ( length xs ) `mod` 2 ==1 = Node  ( buildtree (take ( (length xs) `div` 2 ) xs ) )  (xs!!((length xs) `div` 2 )) ( buildtree (drop ( (length xs) `div` 2 + 1) xs )  ) | otherwise = Node ( buildtree (take ( (length xs) `div` 2 ) xs )  ) (xs!!( (length xs) `div` 2 + 1 )) ( buildtree (drop ( (length xs) `div` 2 + 1) xs )  )


data Expr = Val Int | Add Expr Expr



