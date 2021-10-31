acro xs = [x | x <- xs, elem x ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

pairs = [(x,y)| x <- mySqr, y <- myCube, x < 50, y < 50]

itIsMystery xs =
    map (\x -> elem x "aeiou") xs

y :: [Integer]
y = filter ((==0) . (`mod` 3)) [1..30]

zip :: [a] -> [b] -> [(a,b)]
zip as bs = []