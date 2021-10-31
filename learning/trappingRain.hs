


import Data.List
import Data.Function



heights = [0,1,0,2,1,0,1,3,2,1,2,1]



getRainSize hs =  sum . map getWater . groupBy ((==) `on` fst) . sort $ zip hs [1..]
        where
            getWater arr' = let (items,arr) = unzip arr' in max ((maximum arr) - (minimum arr) - (sum items)) 0