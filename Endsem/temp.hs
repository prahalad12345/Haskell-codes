altmapp f g [] = []
altmap f g (x:xs) = (f x : altmap g f xs )

-- altmap using foldr
altmapHelperr f g = foldr (\(x,y) xs -> if even y then f x:xs else g x:xs) []
altmapr f g xs = altmapHelperr f g (zip xs [0,1..])

-- altmap using foldl
altmapHelperl f g = foldl (\xs (x,y) -> if even y then xs ++ [f x] else xs ++ [g x]) []
altmapl f g xs = altmapHelperl f g (zip xs [0,1..])

--foldM exampleL
ghci> let f = (\xs x -> [x] : [xs])
ghci> foldM f [] [1,2,3]
[[3],[2],[3],[1],[3],[2],[3],[]]