module GCD where
import Prelude hiding (gcd)
-- | GCD
-- >>> gcd 206 40
-- 2
gcd a b = if b == 0 then a else gcd b (a `mod` b)
