awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x


myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

x = (+)
f2 xs = x w 1
    where w = length xs