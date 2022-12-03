emptyList ls | ls==[] = True | otherwise = False
safetail [] = []
safetail (_:ls) = ls
myCompare a b | a==b = "Eq" | a<b = "LT" | a>b = "GT" 
myCompare2 (a,b) | a==b = "Eq" | a<b = "LT" | a>b = "GT"
