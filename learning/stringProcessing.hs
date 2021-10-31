import Data.List




replaceThe :: String -> String
replaceThe = unwords . map fixThe . words
          where
            fixThe "the" = "a"
            fixThe x = x


compress :: (Eq a) => [a] -> [a]
compress = map head . group