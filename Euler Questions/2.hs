main = (print . sum . takeWhile (<= 4000000) . filter even) fibs
    where fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
