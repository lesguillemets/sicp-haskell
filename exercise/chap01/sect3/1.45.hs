module OneFortyfive where
import NewtonsMethod

repeated :: (a -> a) -> Int -> a -> a
repeated _ 0 = id
repeated f n = f . repeated f (n-1)

nthRoot :: Int -> Double -> Double
nthRoot n x = fixP (repeated averaageDamp n' (\y -> x / y^(n-1))) 1.0
    where n' = floor . logBase 2 $ fromIntegral n
-- |
-- >>> import Text.Printf
-- >>> printf "%.4f" $ (nthRoot 23 19)^23
-- 19.0000
