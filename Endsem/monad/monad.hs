import Data.Char  
import Data.List  

main = do line <- fmap reverse getLine 
          linee <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ linee ++ " backwards!"  
