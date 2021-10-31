predicate x = x `mod` 5 == 0 || x `mod` 3 == 0

answer = sum $ filter predicate [1..999]

--  Can also do list comprehension
--- sum [n | n <- [1..999], n `mod` 5 == 0 || n `mod` 3 == 0]
---
main = do
    print answer