import System.IO
import System.Environment

main = do  xargs <- getArgs 
           let ifile = head $ tail xargs 
           let checker = head xargs
           ihandle <- openFile ifile ReadMode 
           contents <- hGetContents ihandle
           putStrLn (unlines $ outputlist checker contents)
           
 

checkword check xs = or [True | x<-xs , x==check]

stringlinestowords input = map words $ lines input 

funccaller checker xs = requiredoutput checker (stringlinestowords xs) 

requiredoutput checker xs = [ x | (x,y) <- zip [1..(length xs)] xs , checkword checker y]

outputlist checker xs = [ (show(x) ++ ": "++(lines xs)!!(x-1)) | x<-funccaller checker xs]

