safetail [] = [] 
safetail (_:xs) = xs

luhndouble x | x*2 < 10 = x*2 
             | otherwise = (x*2 - 10)
