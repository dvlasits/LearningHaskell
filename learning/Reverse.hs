module Reverse where



str = "Curry is awesome"

rvrs :: String -> String
rvrs x = drop 9 x ++ middle ++ take 5 x
        where middle =
                 take 4 $ drop 5 x

main :: IO ()
main = do
    print $ rvrs str