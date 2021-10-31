{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
                | NonFictionBook Nonfiction
                deriving Show

type AuthorName = String

--data Author = Author (AuthorName, BookType)


newtype Goat = Goat Int deriving (Num, Show)

newtype Name = Name String deriving Show
newtype Acres   = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data Car = Car {make :: String
               , model :: String}

type AutoMobile =  Maybe Car