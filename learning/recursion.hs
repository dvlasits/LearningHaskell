module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
    fibonacci (x - 1) + fibonacci (x - 2)


type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
     where go n d count
            | n < d = (count, n)
            | otherwise =
                go (n-d) d (count + 1)


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


addSum :: (Eq a, Num a) => a -> a
addSum 0 = 0
addSum x = x + addSum (x - 1)

mult' :: (Integral a) => a -> a -> a
mult' _ 0 = 0
mult' 0 _ = 0
mult' a b = a + mult' a (b-1)


mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | n <= 100 = mc91(mc91(n + 11))