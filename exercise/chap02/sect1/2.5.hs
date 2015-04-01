module TwoFive where
-- We can represent pairs of nonnegative integers using only numbers and
-- arithmetic operations if we represent the pair a and b as the integer that
-- is the product 2^a * 3^b.

type PairInts = Int
cons :: Int -> Int -> PairInts
cons n m = 2^n*3^m
car :: PairInts -> Int
car x = x `divTimes` 2
cdr :: PairInts -> Int
cdr x = x `divTimes` 3

-- |
-- >>> let n = cons 5 3
-- >>> car n
-- 5
-- >>> cdr n
-- 3

divTimes :: Int -> Int -> Int
divTimes n divisor = iter 0 n
    where
        iter acc m = let (d,mo) = m `divMod` divisor
                         in
                             if mo == 0 then iter (acc+1) d
                                        else acc
