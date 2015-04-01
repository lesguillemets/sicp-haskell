module OneFortytwo where

compose' = (.)
-- just kidding ...
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
-- | ((compose square inc) 6)
-- >>> compose (^2) succ $ 6
-- 49
