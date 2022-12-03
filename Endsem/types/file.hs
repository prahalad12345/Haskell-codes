type Assoc k v = [(k,v)] 
data Move = North | South | East | West
data Shape = Circle Float | Rectangle Float Float
data Nat = Zero | Succ Nat  deriving Show
data List a = Nil | Cons a (List a)
data Tree a = Nill | Node (Tree a) a (Tree a) deriving Show
data Ordering = LTT | EQQ | GTT deriving Show
data Expr = Val Int | Add Expr Expr 

findd :: Eq k => k -> Assoc k v -> v 
findd k t = head [ v | (k',v) <- t, k==k']

move South (x,y) = (x,y-1) 
move North (x,y) = (x,y+1) 
move East (x,y) = (x+1,y) 
move West (x,y) = (x-1,y) 

moves [] p = p
moves (m:ms) p = moves ms (move m p) 

area (Circle r) = 3.14 * r^2 
area (Rectangle x y) = x*y 

safediv _ 0 = Nothing 
safediv m n = Just (m / n)

safehead [] = Nothing 
safehead (x:xs) = Just (x)

nat2int Zero = 0 
nat2int (Succ n) = 1 + nat2int n 

int2nat 0 = Zero 
int2nat n = Succ (int2nat (n-1))

addd m Zero = m 
addd m (Succ(n)) = Succ ( addd m n )

multt m Zero = Zero
multt m (Succ(n)) = addd m (multt m n)

headd Nil = 0 
headd (Cons x _) = x

occurs x Nill = False 
occurs x (Node a b c) = (b==x) || occurs x a || occurs x c

flatten Nill = [] 
flatten (Node a b c) = [b] ++ flatten a ++ flatten c 

comparee a b | a>b = GTT 
             | a<b = LTT
             | otherwise = EQQ

succc (Node Nill y _) = y              
succc (Node ltree y _) =  succc ltree        

balance [] = Nill 
balance [x] = Node Nill x Nill
balance xs = Node (balance (take ((length xs) `div` 2) xs)) (xs!!((length xs) `div` 2)) (balance (drop (((length xs) `div` 2) + 1) xs))

folde f g (Val a) = f a 
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval = folde id (+)
size = folde (const 1) (+)

treeinsert x Nill = Node Nill x Nill 
treeinsert x (Node ltree a rtree) | x<=a = (Node (treeinsert x ltree) a rtree)
                                  | x>a =  (Node ltree a (treeinsert x rtree))

treedelete x Nill = Nill 
treedelete x (Node Nill y rtree ) | x==y = rtree 
                                  | x>y =  (Node Nill y (treedelete x rtree) )
                                  | otherwise = (Node Nill y rtree )
treedelete x (Node ltree y Nill ) | x==y = ltree 
                                  | x>y =  (Node ltree y Nill)
                                  | otherwise = (Node (treedelete x ltree) y Nill )
treedelete x (Node ltree y rtree) | x==y = (Node ltree (succc rtree) (treedelete (succc rtree) rtree) )
                                  | x>y =   (Node ltree y (treedelete x rtree) )
                                  | otherwise = (Node (treedelete x ltree) y rtree )
