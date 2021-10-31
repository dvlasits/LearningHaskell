import Data.Char (digitToInt)
import Data.List


input = "0100010101"


out input  = sum . map (uncurry min) . zip (tail grouped) $ grouped
    where grouped = map length . group . map digitToInt $ input

--do horea question on summing mod 60
--do today question on min cover Set
--do yesterdays question on min index that is run accross most




-- The fountain addendum

fountains = [1, 1, 2, 1, 1,0]





fout fin' = 
    let 
        recsolve arr pos 
            | null arr = 0
            | pos >= n = 0
            | otherwise = 
                let (ops, rest) = span ((<=pos + 1) . fst) arr
                in 1 + recsolve rest (maximum . map (snd) $ ops)
    in
        recsolve fin 0
    where
        n = (genericLength fin')
        fin = sort [(index - reach, index + reach) | (index, reach) <- zip [1..] fin']













