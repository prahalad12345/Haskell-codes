import System.IO
import System.Environment
-- xargs takes care of the argument
main = do  xargs <- getArgs 
           let ifile = head xargs 
           --reading the file as an input
           ihandle <- openFile ifile ReadMode 
           --contents are read from the file
           contents <- hGetContents ihandle
           -- removewhitesapce takes the whole content and removes any unwanted white spaces
           putStrLn (removewhitespace contents)
           
removewhitespace str = [x | x <- str , (elem x ['\n','\t',' ']==False)]
           
