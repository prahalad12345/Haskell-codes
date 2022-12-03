evenn x = if x `rem` 2 ==0 then return True else return False
signnum x= if x>0 then  1 else if x<0 then -1 else 0
anothersignum x | x>0 = 1 | x==0 = 0 | otherwise= -1
