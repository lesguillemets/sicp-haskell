module OneEleven where

-- A function f is defined by the rule that f(n) = n if n < 3
-- and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3.
-- Write a procedure that computes f by means of a recursive process.
-- Write a procedure that computes f by means of an iterative process.

-- | f is the literal translation.
-- >>> all (\n -> f n == f' n) [1..20]
-- True
--
-- | f' is fast.
-- >>> f' 200
-- 429285341777505086291879477170078944250881374726914954774217395887598980875

f n = if n < 3 then n else f (n-1) + 2*f(n-2) + 3*f(n-3)

f' n = if n < 3
           then n
           else fAcc 2 1 0 n
           where
               fAcc _ _ c 0 = c
               fAcc a b c n = fAcc (a+2*b+3*c) a b (n-1)
