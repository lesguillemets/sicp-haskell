module TwoSeventeen where
import SchemeList

lastPair :: SchList a -> a
lastPair xs = case cdr xs of
                  Nil -> car xs
                  xs' -> lastPair xs'
-- |
-- >>> lastPair (scL [2,3,4,5,1])
-- 1
