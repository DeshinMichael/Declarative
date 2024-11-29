> newtype Union a = Union { getUnion :: [a] } deriving (Eq, Show)

> instance Eq a => Semigroup (Union a) where
>  Union xs <> Union ys = Union (xs `union` ys)
>    where
>      union [] ys = ys
>      union (x:xs) ys
>        | x `elem` ys = union xs ys
>        | otherwise   = x : union xs ys

> instance Eq a => Monoid (Union a) where
>  mempty = Union []
>  mappend = (<>)

> unique :: Eq a => [a] -> [a]
> unique xs = getUnion $ mconcat listOfUnion
>  where
>    singleton x = [x]
>    listOfUnion = fmap (Union . singleton) xs