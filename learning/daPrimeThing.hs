
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

primes = 2 : filter (null . tail. primeFactors) [3,5..]


divUntil p = head . dropWhile ((==0) . (`mod` p)) . iterate (`div` p)


primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | n == 1 = []
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (divUntil p n) (p:ps)
        | otherwise      =     factor n ps

myFactors = M.fromList $ take (2 * 10^5) $ zip [2..] $ map (primeFactors) [2..]

myNums = take (10^5) [(10^5),(10^5)..]
numCounts nums = M.fromListWith (+) . flip zip [1,1..] . concat . map (primeFactors) $ nums