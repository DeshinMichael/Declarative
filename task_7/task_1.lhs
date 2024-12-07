> fmap' :: Monad m => (a -> b) -> m a -> m b
> fmap' f ma = ma >>= (\a -> return (f a))

> pure' :: Monad m => a -> m a
> pure' = return

> ap' :: Monad m => m (a -> b) -> m a -> m b
> ap' mf ma = mf >>= (\f -> ma >>= (\a -> return (f a)))