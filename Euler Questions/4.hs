import Data.List (maximum)

allProds = [x * y | x <- [100..999], y<-[100..x], palin (x * y)]

palin x = let y = show x in y == reverse y

main = 
    (print . maximum) allProds