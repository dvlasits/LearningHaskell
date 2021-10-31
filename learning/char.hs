import Data.Char

ups = filter isUpper

capit [] = []
capit (x:xs) = toUpper x : capit xs

firstCapit = toUpper . head


z :: String
z = foldl (combine) "" [1..5]
    where 
        combine :: String -> Integer -> String
        combine st = (st ++) . show 

foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs