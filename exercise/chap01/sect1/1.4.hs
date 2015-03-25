module OneFour where
-- Observer that our model of evaluation allows for combinations whose
-- operators are compound expressions.
-- (define (a-plus-abs-b a b)
--   ((if (> b 0) + -) a b))
-- | You can do this in haskell, too.
-- >>> aPlusAbsB 3 5
-- 8
-- >>> aPlusAbsB 3 (-3)
-- 6

aPlusAbsB :: (Num a, Ord a) => a -> a -> a
aPlusAbsB a b = (if b > 0 then (+) else (-)) a b
