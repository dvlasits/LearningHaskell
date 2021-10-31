primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors x = primeFactors' x primes
    where 
        primeFactors' x (p:ps)
            | p*p > x = [x]
            | x `mod` p == 0 = p : primeFactors' (x `div` p) (p:ps)
            | otherwise = primeFactors' x ps

main = print . sum . takeWhile (<2000000) $ primes