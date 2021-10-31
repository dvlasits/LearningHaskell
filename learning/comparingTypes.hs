{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances#-}




class TooMany a where
    tooMany :: a -> Bool



instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
    tooMany (x, x') = tooMany (x + x')

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, x') = tooMany x'



newtype Goats = Goats Int deriving (TooMany, Show)
newtype New = New (Int, String) deriving (Show, TooMany)


data Person = 
    Person { name :: String
           , age :: Int    }
           deriving (Eq, Show)

jm = Person "julie" 108 
ca = Person "chris" 16