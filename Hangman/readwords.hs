import System.IO
import System.Environment

main = do  xargs <- getArgs
           let ifile = head xargs 
               ofile = head.tail $ xargs
           ihandle <- openFile ifile ReadMode 
           ohandle <- openFile ofile WriteMode 
           contents <- hGetContents ihandle
           hPutStr ohandle $ unlines . filter (\xs -> length xs >=6 ) . filter  (and . map (flip elem $ ['a'..'z'])) . lines $ contents
           hClose ihandle 
           hClose ohandle 
                             
