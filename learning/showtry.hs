import Data.List

data Mood2 = Blah2

instance Show Mood2 where
    show _ = "Blah"


data Person = Person Bool deriving (Show)
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)



data Mood = Blah
        | Woot deriving (Show, Eq)
settleDown x = 
    if x == Woot 
    then Blah
    else x


type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"



data Rocks =
    Rocks String deriving (Eq, Show, Ord)

data Yeah =
    Yeah Bool deriving (Eq, Show, Ord)

data Papu =
    Papu Rocks Yeah deriving (Eq, Show, Ord)

equalityForall :: Papu -> Papu -> Bool 
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'


--i :: a
--i = 1

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aTob int a = (aTob a) + (fromIntegral int)

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

addOneIfOdd n =
    case odd n of 
        True -> (\x -> x + 1) n
        False -> n

addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (\x -> x + 5) (if x > y then y else x)

myflip f = \x -> \y -> f y x

myflip2 f x y = f y x
