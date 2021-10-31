


myAbs :: Integer -> Integer
myAbs x
    | x <  0 = (-x)
    | otherwise = x


bloodNa :: Integer -> String 
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"


isRight :: (Num a, Eq a) =>
                a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "Yep"
    | otherwise = "nope"



dogYrs :: Integer -> Integer 
dogYrs x
    | x <= 0 = 0
    | x <= 1 = x * 15
    | x <= 2 = x * 12
    | x <= 4 = x * 8
    | otherwise = x * 6



pal xs
    | xs == reverse xs = True 
    | otherwise = False

numbers x
    | x < 0  = -1
    | x == 0 =  0 
    | x > 0  =  1
