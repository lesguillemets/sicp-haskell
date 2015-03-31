module OneThirtynine where
import Contfrac (contFrac')

tanCf :: Int -> Double -> Double
tanCf k x =  contFrac' (fromIntegral . (subtract 1) . (*2)) f k
    where
        f n = if n > 1 then -x*x else x

-- | Seems fast.
-- >>> import Text.Printf
-- >>> let t = tanCf 10
-- >>> printf "%.14f" . abs $ t 0.3 - tan 0.3
-- 0.00000000000000
-- >>> printf "%.14f" . abs $ t 1 - tan 1
-- 0.00000000000000
