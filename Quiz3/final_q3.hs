--I went back and worked on the same code and find the small mistake in my code
main  =  do line <- getLine 

            let allwords = tail $ words line 
            let firstnum = head $ words line 
            let num = read firstnum::Int 
            let ans = solve allwords [num, 0 , 1] 
            print ans 
            main 
            




solve []  xs = head xs
solve (x:xs) (a:b:ys) | x == "+" = solve  xs ((a+b):drop 2 (a:b:ys)) 
                      | x == "-" = solve  xs ((b-a):drop 2 (a:b:ys)) 
                      | x == "*" = solve  xs ((a*b):drop 2 (a:b:ys))  
                      | x == "/" = solve  xs ((b `div` a):drop 2 (a:b:ys))   
                      | otherwise = solve xs ((read x::Int):a:b:ys)



