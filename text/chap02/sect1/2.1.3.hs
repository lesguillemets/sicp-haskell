module WhatIsMeantbyData where
-- We can use Either, but for simplicity ...
cons :: a -> a -> Int -> a
cons x y = let dispatch m = case m of
                                0 -> x
                                1 -> y
                                _ -> error "Argument not 0 or 1"
            in dispatch

car z = z 0
cdr z = z 1

-- |
-- >>> let a = cons 3.2 5
-- >>> car a
-- 3.2
-- >>> cdr a
-- 5.0
