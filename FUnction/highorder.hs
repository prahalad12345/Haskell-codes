takewhi _ []= []
takewhi f (x:xs) | f x = x:(takewhi f (xs)) | otherwise = []


