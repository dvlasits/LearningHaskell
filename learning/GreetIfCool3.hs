
module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    case cool of
        True ->
            putStrLn "eyyyyyy. What's shakin'?"
        False ->
            putStrLn "pshhhhhh"
        where cool = 
                coolness == "downright frosty yo"

functionC :: Ord p => p -> p -> p
functionC x y = if (x > y) then x else y

funcionC' :: Ord p => p -> p -> p
funcionC' x y =
    case x > y of
        True -> x
        False -> y

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' :: Integral p => p -> p
ifEvenAdd2' n = 
    case even n of
        True -> n+2
        False -> n
nums :: (Ord a, Num a, Num p) => a -> p
nums x =
    case compare x 0 of
        LT -> -1 
        GT -> 1
        EQ -> 0