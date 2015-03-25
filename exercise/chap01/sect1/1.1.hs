module OneOne where

-- | Below is a sequence of expressions.
-- | What is the result printed by the interpreter in response to each
-- | expression?
-- | Assume that the sequence is to be evaluated in the order in which
-- | it is presented.
-- >>> 10
-- 10
-- >>> plus [5,3,4]
-- 12
-- >>> (-) 9 1
-- 8
-- >>> (+) ((*) 2 4) ((-) 4 6)
-- 6
-- >>> let a = 3
-- >>> let b = (+) a 1
-- >>> plus [a, b, (*) a b]
-- 19
-- >>> (==) a b
-- False
-- >>> if and [(>) b a, (<) b ((*) a b)] then b else a
-- 4
-- >>> f a b
-- 16
-- >>> (+) 2 (if (>) b a then b else a)
-- 6
-- >>> (*) (g a b) ((+) a 1)
-- 16

plus :: (Num a) => [a] -> a
plus = foldl1 (+)

f a b
    | (==) a 4 = 6
    | (==) b 4 = plus [6,7,a]
    | otherwise = 25

g a b
    | (>) a b = a
    | (<) a b = b
    | otherwise = -1

