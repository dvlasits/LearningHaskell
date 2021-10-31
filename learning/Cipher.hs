module Cipher where

import Data.Char
import Data.Bool

-- :t chr which is Int -> Char
--


ceaserCipher :: Int -> String -> String 
ceaserCipher toShift = map shift
    where
        shift :: Char -> Char
        shift = chr . (+97) . (`mod` 26) . (toShift - 97 +) . ord
