module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int 
addOnePF = (+1)



main :: IO () 
main = do
    print (0 :: Int) -- 0
    print (add 1 0) -- 1
    print (addOne 0) -- 1
    print (addOnePF 0) -- 1
    print ((addOne . addOne) 0) -- 2
    print ((addOnePF . addOne) 0) -- 2
    print ((addOne . addOnePF) 0) -- 2
    print ((addOnePF . addOnePF) 0) -- 2
    print (negate (addOne 0)) -- -1
    print ((negate . addOne) 0) -- -1
    print ((addOne . addOne . addOne
           . negate . addOne) 0) -- 2

print' :: (Show a) => a -> IO ()
print' = putStrLn . show



tensDigit :: Integral a => a -> a 
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where (xLast, _) = x `divMod` 100
          (_, d) = xLast `divMod` 10


foldBool :: a -> a -> Bool -> a
foldBool a a' t = 
    case t of
        True -> a
        False -> a'

foldBool' :: a -> a -> Bool -> a
foldBool' a a' t 
    | t = a
    | t = a'

g :: (a -> b) -> (a, c) -> (b, c)
g aTob (a, c) = (aTob a, c)

 
