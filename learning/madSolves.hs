import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)



import qualified Data.Set as Set





--Solution to 1
countGroups :: Integer -> [Integer] -> Int
countGroups maxSpread = length . groupBy ((<=) . (+maxSpread)) . sort


--Solution to 3

modVal = 10^9 + 7

calcRow row above = newRow
    where newRow = zipWith3 (\ x y z -> ((x + y) * z)) (0 : newRow) row above

numWays :: [[Integer]] -> Integer
numWays = last . foldl' calcRow (1 : [0,0..])





--numWays'' = tail . scanl (\xs b -> fix (zipWith (*) b . zipWith (+) xs . (0:))) (1 : repeat 0)


--Solution to 2

type IdsAndCounts = Set (Integer, Integer)



{-insertTo :: Map String IdsAndCounts -> (Integer,String) -> Map String IdsAndCounts
insertTo map (index,sentence) = foldl (func) map (words sentence)
     where
        func :: Map String IdsAndCounts -> String -> Map String IdsAndCounts
        func map word= Map.insert word newSet map
            where 
                out :: (Integer, Integer)
                out = (index, (fromIntegral . length . filter (==word) . words) sentence)
                newSet :: IdsAndCounts
                newSet = case Map.lookup word map of
                    Nothing -> Set.singleton out
                    Just oldSet -> Set.insert out oldSet
        



createMap :: [String] -> Map String IdsAndCounts
createMap sentences = foldl (insertTo)  Map.empty (zip [0..] sentences)

solveQuery :: [String] -> Map String IdsAndCounts -> [Integer]
solveQuery q bigMap = sort . 

solve sentences queries = map solveQuery q bigMap
    where 
        bigMap = createMap sentences
        qs = map words queries-}