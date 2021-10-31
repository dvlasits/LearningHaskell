
data Employee = Coder
                | Manager
                | Veep
                | CEO
                deriving (Eq, Ord, Show)


reportBoss :: Employee -> Employee -> IO () 
reportBoss e e' = putStrLn $ show e ++
                     " is the boss of " ++
                     show e'

employeeRank :: Employee -> Employee
                         -> IO ()
employeeRank e e' =
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither Better"
        LT -> flip reportBoss e e'

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10 

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1 

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2
