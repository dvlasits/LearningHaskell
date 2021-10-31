
import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                    deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem] 
theDatabase =
     [ DbDate (UTCTime
                 (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123)) , DbNumber 9001
     , DbString "Hello, world!"
     , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
     ]
    
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate x) -> x)  . filter isDbDate
    where
        isDbDate (DbDate _) = True
        isDbDate _ = False


{-foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs-}

nouns  = ["Daniel", "Tree", "Legend"]
verbs = ["runnig", "playing", "eating"]

combos = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- gets average lenght of worrd
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr ((||) . p) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (==x)

myElem3 :: Eq a => a -> [a] -> Bool
myElem3 = (myAny) . (==)

myReverse :: [a] -> [a] 
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (choose) []
    where 
        choose item arr = if p item then item : arr else arr


squish :: [[a]] -> [a]
squish = foldr (++) []

data Doggies a = 
    Husky a
    | Mastiff a 
    deriving (Eq, Show)


data DogueDeBordeaux doge =  DogueDeBordeaux doge