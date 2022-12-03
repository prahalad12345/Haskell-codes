main = interact $ unlines . filter (\xs -> length xs < 10) . lines  
--sequence . map = mapM
temp = sequence_ $ (map (putStrLn.show) [1,2,3])