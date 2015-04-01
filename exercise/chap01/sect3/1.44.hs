module OneFortyfour where

smooth :: (Fractional a) => Double -> (Double -> a) -> Double -> a
smooth dx f x = (f (x-dx) + f x + f (x+dx))/3
repeated :: Int -> (a -> a) -> a -> a
repeated = undefined -- see 1.43

smootheN :: (Fractional a) => Int -> Double -> (Double -> a) -> Double -> a
smootheN n dx = repeated n (smooth dx)
