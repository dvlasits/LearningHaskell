import Data.Char (toUpper)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf l@(x:xs) (y:ys)
            | x == y = isSubseqOf xs ys
            | otherwise = isSubseqOf l ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitTuple . words
            where
            capitTuple s@(x:xs) = (toUpper x : xs, s)

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph ('.':' ':x:xs) =  '.':' ': toUpper x : xs
capitalizeParagraph (x:xs) = x : capitalizeParagraph xs