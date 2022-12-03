data Either' a b = Left' a | Right' b deriving (Eq,Ord,Show ,Read) 

instance Functor (Either' a ) where 
   fmap f (Left' x) = (Left'  x  )
   fmap f (Right' x) = ( Right'  x  )
