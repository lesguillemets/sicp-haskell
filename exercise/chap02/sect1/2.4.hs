module TwoFour where

-- this is beautiful ...
cons x y m = m x y
car z = z (\ p _ -> p)
cdr z = z (\ _ q -> q)

-- |
-- >>> let a = cons 3 5
-- >>> car a
-- 3
-- >>> cdr a
-- 5
