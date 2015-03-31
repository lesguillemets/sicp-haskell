module OneThirtyeight where
import Contfrac (contFrac')

euler :: Int -> Double
euler = (+ 2) .  contFrac' (fromIntegral . dis) (const 1)

-- | Seems fast.
-- >>> import Text.Printf
-- >>> printf "%.14f" $ euler 30
-- 2.71828182845905

dis :: Int -> Int
dis n = let (d,m) = n `divMod` 3 in
    if m == 2 then 2 * (d+1)
              else 1
