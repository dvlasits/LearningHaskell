data Optional a = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
   
instance Semigroup a => Semigroup (Optional a) where
    Nada <> Nada = Nada
    (Only x) <> (Only x') = Only (x <> x')
    Nada <> o@(Only _) = o
    o@(Only _) <> Nada = o
    