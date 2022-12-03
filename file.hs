import System.IO

main = do fileName <- getLine 
	  handle <- openFile fileName ReadMode 
	  contents <- hGetContents handle--takes all the input from the file 
	  print $length contents
	  hClose handle
