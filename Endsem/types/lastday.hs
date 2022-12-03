data Person = Person String String Int Float String String deriving (Show)

data Car = Car String String Int deriving (Show)

data Vector a  = Vector a a a deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) 
-- a = 3 :-: 4 :-: 5 :-: Empty 
--  100 :-: a   
-- result : 100 :-: (3 :-: (4 :-: (5 :-: Empty)))

infixr 5 .++ 
Empty .++ ys = ys 
(x:-:xs) .++ ys = x:-: (xs .++ ys)


tellCar (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y 
 

vplus :: (Num t) => Vector t -> Vector t -> Vector t 
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

