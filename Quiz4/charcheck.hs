import System.IO
import System.Environment
import Data.Char

main = do  xargs <- getArgs 
           let ifile = head xargs 
           ihandle <- openFile ifile ReadMode 
           contents <- hGetContents ihandle
           
           putStrLn ( unlines $ result $ map (map toLower) $ lines contents)
           




result xs = [ vowel x  | x <- xs]
vowel x = [ a | a<-x , elem a ['a','e','i','o','u']]
           
           
