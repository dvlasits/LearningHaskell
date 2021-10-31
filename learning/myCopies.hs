import Data.Bool

myAnd :: [Bool] -> Bool 
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myAny'
     where
         myAny' [] = False
         myAny' (x:xs) = f x || myAny' xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem i (x:xs) =
    i == x || myElem i xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' i arr = myAny (==i) arr


myReverse :: [a] -> [a]
myReverse arr = myReverse' [] arr
    where
        myReverse' acc [] = acc
        myReverse' acc (x:xs) = myReverse' (x:acc) xs

squish :: [[a]] -> [a]
squish = foldl (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a] 
squishAgain = squishMap id



myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy comp as = foldl1 bigger as
    where
        bigger x y = bool x y (comp y x == GT)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp = myMaximumBy (flip comp)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
