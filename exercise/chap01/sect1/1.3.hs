module OneThree where
import Data.List (sortBy)
-- | Define a procedure that takes three numbers as arguments
-- | and returns the sum of the squares of the two larger numbers.
-- >>> f 3 4 2
-- 25
-- >>> f 2 7 3
-- 58

f :: (Num a, Ord a) => a -> a -> a -> a
f a b c = sum . map (^2) . take 2 . sortBy (flip compare) $ [a,b,c]
