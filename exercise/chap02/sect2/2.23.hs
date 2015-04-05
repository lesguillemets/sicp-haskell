module TwoTwentythree where
-- cf. mapM_
forEach :: (Monad m) => (a -> m b) -> [a] -> m ()
forEach _ [] = return ()
forEach procedure (x:xs) = do
        _ <- procedure x
        forEach procedure xs

-- |
-- >>> forEach (putStrLn . show) [3,4,21]
-- 3
-- 4
-- 21
