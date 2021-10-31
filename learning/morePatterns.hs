module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y


addEmUp2Alt :: Num a => (a, a) -> a 
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a 
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c 
third3 (_, _, x) = x




k :: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k ((4-1), 10)

k2 :: String
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f)
     -> ((a, d), (c, f))
f (a, b, c) (d, e, f) =
    ((a, d), (c, f))


--if x + 1 == 1 then "AWESOME" else "wut"
funcZ x =
    case x + 1 == 1 of
        True -> "AWESOME" 
        False -> "wut"

pal xs = 
    case xs == reverse xs of
        True -> "yes"
        False -> "no"

pal' xs =
    case y of
        True -> "yes"
        False -> "no"
    where y = xs == reverse xs

