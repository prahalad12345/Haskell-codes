import Data.Char 
import Data.List 
import Control.Applicative

data CMaybe a = CNothing | CJust Int a deriving (Show)

data Expr = Val Int | Div Expr Expr 

instance Functor CMaybe where 
         fmap f CNothing = CNothing  
         fmap f (CJust counter x) = CJust (counter+1) (f x)
{- 
class (Functor f) => Applicative f where 
      pure:: a -> f a 
      (<*>) :: f (a->b) - > f a -> f b 
      

instance Applicative Maybe where
         pure = Just 
         Nothing <*> _ = Nothing 
         (Just f) <*> something = fmap f something
-}
 
main = do line <- fmap  (intersperse '-' . reverse . map toUpper) getLine 
          putStrLn line 

eval (Val n) = Just n 
eval (Div x y) = do xx <- eval x 
                    yy <- eval y 
                    yyy <- (safediv xx yy)
                    return yyy

evall (Val n) = pure n
evall (Div x y) = evall x >>= \n -> 
                  evall y >>= \m ->
                  safediv n m

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

