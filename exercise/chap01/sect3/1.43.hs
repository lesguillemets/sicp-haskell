module OneFortythree where

repeated :: Num a => (a -> a) -> Int -> a -> a
repeated _ 0 = id
repeated f n = f . repeated f (n-1)
-- |
-- >>> repeated (^2) 2 $ 5
-- 625
