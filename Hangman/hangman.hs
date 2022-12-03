import System.IO
import System.Environment

main = do  let ifile = "words1" 
--create a file handle to read the file 
           ihandle <- openFile ifile ReadMode 
--get all the content in the file and read it 
           contents <- hGetContents ihandle
--use lines to convert to a list and then extracting the third string
           let val = ( lines $ contents ) !! (4)
--generating a _______ string of length of the 3rd word selected. 
           let str = strgenerator (length val )
           do putStr "Start String " 
           do putStrLn str
--calling for 14 iteration assuming that \n will be given by the user .
           eachiteration 14 str val
--closing the file       
           hClose ihandle 
                          
                          
--generating the string of ______                          
strgenerator n = ['_' | i<-[1..n] ] 
  
--check if all the places are filled and are same as the rest
checkifallfound [] [] = True
checkifallfound (x:xs) (y:ys) = x==y && checkifallfound xs ys
--check if the words are found and matches the start word
checkifin [] [] _ = []
checkifin (x:xs) (y:ys) ch | ch==x  = (ch:(checkifin xs ys ch)) | otherwise = (y:(checkifin xs ys ch))

--if the number of iteration is 0 then check if have reached conclusion if yes then positive message then the negative message
eachiteration 0 str val | checkifallfound str val = putStrLn "\nPhew ,That was Close !" | otherwise = putStrLn ("\nYou Failed!" ++ show val)

 
eachiteration n str val = do x<-getChar   
                             --get an input and fill it in the start string 
                             let st = checkifin val str x
                             --if the character input is not '\n'the print the string or else just print a new line 
                             if x/= '\n' then
                                do putStr st
                             else 
                                do putStr "\n"
                             --if all the words are found then print the message 
                             if checkifallfound st val then 
                                do putStrLn "\nGot the Word!!" 
                             else 
                             --else cal teh function recursively again .
                                do eachiteration (n-1) st val 
                                   
                             
                                       
                      

