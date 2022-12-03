--main = putstr "hello world"
main = do putstr "What you name?"
          line <- getLLine
          putstr line

--compile using ghc helloworld.hs 
--helloworld to run the code 

putstr [] = return ()
putstr (x:xs) = do putChar x 
	           putstr xs 
--Alternative	           
--putstr xs = do if null xs 
--		 then 
--		 return () 
--		 else 
--               do
--		 putChar (head xs) 
--		 putstr (tail xs)


getLLine = do c <- getChar 
              if c == '\n'  
                 then 
                  do return []
                 else 
                  do cs<-getLLine 
                     return (c:cs)
                     
act :: IO(Char,Char) 
act = do x<- getChar
         getChar
         y<-getChar
         return (x,y)
