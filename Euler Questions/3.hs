largestFactor x = largestFactor' x 2
    where 
    largestFactor' x num 
        | num == x = num
        | x `mod` num == 0 = largestFactor' (x `div` num) num
        | otherwise = largestFactor' x (num + 1)




main = print $ largestFactor 600851475143