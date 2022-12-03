hammingDist xs ys | length xs /= length ys = Nothing --if the length is unequal then there is no purpose to evaluate so retutn nothing
                  | otherwise = Just (sum [1 | (x,y) <- zip xs ys , x/=y]) -- otherwise use list comprehension to sum up to 1 and return the  Just of sum
                  
                  
