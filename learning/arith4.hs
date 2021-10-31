roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read . show $ a

main = do
    print ((roundTrip 4) :: Integer)
    print (id 4)

