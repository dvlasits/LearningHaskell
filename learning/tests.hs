munge :: (x -> y)
    -> (y -> (w, z))
    -> x
    -> w



munge xToy yTowz x = fst . yTowz . xToy $ x
