Maybe x = Just x | Nothing 

--change x to Int or some other type.

fmap f Nothing = Nothing 
fmap f (Just x) = Just (f x)  


inc [] = [] 
inc (x:xs) = (x+1 | inc xs)
