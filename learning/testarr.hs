--{-# LANGUAGE AllowAmbiguousTypes #-}
import qualified Data.Vector as V
import qualified Data.Sequence as S

a = V.fromList [x^2 | x <- [1..50000001]]

b = S.fromList [x^2 | x <- [1..50000001]]

y = [x^2 | x <- [1..1000000000]]

rem (Just x) = x

main :: IO ()
main = do
    print (S.lookup 1000000 b)
    print (S.lookup 1000000 b)
    print (S.lookup 10000000 b)
    print(sum y)