import Data.List
import Data.Map (Map)
import qualified Data.Map as M

f l r 
    | l < 0 || r < 0 = []
    | l == 0 && r == 0 = [""]
    | otherwise = out1 ++ out2
    where 
        out1 = map ("l"++) (f (l-1) r)
        out2 = map ("r"++) (f l (r-1))



partiton xs item = filter (<item) xs ++ filter (>=item) xs

remDups2 :: [Integer] -> [Integer]
remDups2 = map head . group

singles :: Ord k => [k] -> [k]
singles = map fst . M.toList . M.fromList . flip zip [1,1..]

joinIntervals = joinIntervals' . sort
    where
        joinIntervals' 