module HalfInterval where

halfIntervalMethod :: (Ord a, Fractional a)
    => (a -> a) -> a -> a -> Maybe a
halfIntervalMethod f a b
    | aValue < 0 && 0 < bValue = Just $ searchZero f a b
    | bValue < 0 && 0 < aValue = Just $ searchZero f b a
    | otherwise = Nothing
    where
        aValue = f a
        bValue = f b
-- | Let's try.
-- >>> halfIntervalMethod sin 2.0 4.0
-- Just 3.14111328125
-- >>> halfIntervalMethod (\x -> x^3 - 2*x - 3) 1.0 2.0
-- Just 1.89306640625

searchZero :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a
searchZero f negPoint posPoint
    = let midPoint = average negPoint posPoint
          closeEnough = closeEnoughWith 0.001
          in
              if closeEnough negPoint posPoint
                  then midPoint
                  else
                      case f midPoint `compare` 0 of
                          GT -> searchZero f negPoint midPoint
                          LT -> searchZero f midPoint posPoint
                          EQ -> midPoint

closeEnoughWith :: (Num a, Ord a) => a -> a -> a -> Bool
closeEnoughWith threshold x y = abs (x-y) < threshold

average :: Fractional a => a -> a -> a
average = ( (/2) .) . (+)
