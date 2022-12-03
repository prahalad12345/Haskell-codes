import System.IO
import System.Environment

main = do  xargs <- getArgs
           let ifile = head xargs 
           ihandle <- openFile ifile ReadMode 
           contents <- hGetContents ihandle
           let ans = removecomments contents
           putStrLn ans
           
removecomments = unlines.filter (\x -> ((x !! (1) /= '/'))).filter (\x -> ((x !! (0) /= '/'))).lines
           
        	
