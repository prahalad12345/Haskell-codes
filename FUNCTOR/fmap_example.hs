data Maybbe a = Nothhing | Justt a deriving Show 
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Maybbe where 
         fmap _ Nothhing = Nothhing 
         fmap g (Justt x) = Justt (g x)
         
instance Functor Tree where 
         fmap g (Leaf x) = Leaf (g x)
         fmap g (Node l r) = Node (fmap g l) (fmap g r) 
         
{-instance Functor IO where  
         fmap g mx = do { x <- mx ; return (g x) } -}
         
instance Applicative Maybbe where 
         pure = Justt
         Nothhing <*> _  = Nothhing 
         (Justt g) <*> mx = fmap g mx 
         
{-instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result) -}
         
       
--getchars  using applicative 

getChars 0 = return [] 
getChars n = pure (:) <*> getChar <*> getChars (n-1) 
{-
sequenceAA []  = pure [] 
sequenceAA (x:xs) = pure (:) <*> x <*> sequenceAA xs
-}
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  
          
 
