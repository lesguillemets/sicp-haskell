module Exponentiation where

expt :: (Num a, Integral b) => a -> b -> a
expt _ 0 = 1
expt b n = b * expt b (n-1)

expt' :: (Num a, Integral b) => a -> b -> a
expt' b n = exptIter b n 1
exptIter :: (Num a, Integral b) => a -> b -> a -> a
exptIter _ 0 acc = acc
exptIter b n acc = exptIter b (n-1) (acc*b)

fastExpt b n
    | n == 0 = 1
    | even n = (fastExpt b (n `div` 2) )^2
    | otherwise = b * fastExpt b (n-1)
