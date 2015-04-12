module TwoThirtyfour where
import Data.List

hornerEval :: (Num a) => a -> [a] -> a
hornerEval x (a0:as) = a0 + (foldl' (((* x).)  . (+)) 0 . reverse) as

f :: Int -> Int
f x = 1 + 3*x + 5*x^3 + x^5
-- |
-- >>> hornerEval 2 [1,3,0,5,0,1]
-- 79
-- >>> f 2
-- 79
