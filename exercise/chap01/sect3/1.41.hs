module OneFortyone where

double :: (a -> a) -> a -> a
double f n = f (f n)
-- double = join (.)
-- double f = f . f

-- (((double (double double)) inc) 5
-- double (double double)
-- = (double double) . (double double)
-- = double . double . double . double
-- | Let's try.
-- >>> double (double double) succ 5
-- 21
--
-- | note the difference from ((double (double (double inc))) 5)
-- >>> (double . double . double) succ $ 5
-- 13
