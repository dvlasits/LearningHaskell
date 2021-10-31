import Data.Map (Map)
import qualified Data.Map as M
import Data.Graph (Graph)
import qualified Data.Graph as G
import qualified Data.Tuple as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Function
import qualified Data.List as L
import Graph



-- parent to child
parentChild :: [(Int,Int)]
parentChild = [(1,2),(3,2),(9,2),(5,1)]

fixing = map (\(x,y) -> (y, [x]))

adjList = M.fromListWith (++) parentChild'
    where 
        parentChild' = fixing parentChild

zeroOne = map doStuff [0,1]
    where 
        doStuff x = M.filter ((==x) . length) adjList

lookup' x = M.findWithDefault [] x adjList 


ancestors y = ancestors' (lookup' y)
    where
        ancestors' [] = []
        ancestors' (x:xs) = [x] ++ (ancestors' (lookup' x ++ xs)) 

commonAncestors x y = not . S.null $ (S.intersection `on` setted) x y
    where
        setted = S.fromList . ancestors

oldest x = snd $ oldest' x
    where
        oldest' x
            | L.null . lookup' $ x = (-1, x)
            | otherwise = let 
                    (dist, item) = maximum . map oldest' . lookup' $ x
                        in (dist + 1, item)







myGraph parentChild = G.buildG (0,maxs) parentChild'
    where
        maxs = maximum . map (uncurry max) $ parentChild
        parentChild' = map T.swap parentChild

-- zeroOne :: Graph -> ([Int],[Int])
-- zeroOne g = (mfilter 0 od, mfilter 1 od)
--     where 
--         od = G.outdegree g
--         mfilter :: Int -> ([(Int,Int)] -> [Int])
--         mfilter x = map fst . filter ((==x) . snd)