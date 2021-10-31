--Trying an erasthones Sieve


primes' (x:xs) = x : (primes' $ filter ((/=0) . (`mod` x)) xs)
primes = primes' [2..]
main = print $ primes !! 10001