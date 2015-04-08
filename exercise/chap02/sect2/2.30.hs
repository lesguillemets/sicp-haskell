module TwoThirty where

import TList

squareTree :: (Num a) => TList a -> TList a
squareTree = schmap (^2)

squareTree' :: (Num a) => TList a -> TList a
squareTree' Nil = Nil
squareTree' (V x) = V (x*x)
squareTree' (Cons p q) = Cons (squareTree' p) (squareTree' q)

-- |
-- >>> let example = list [V 4, fromList [3,4], V 2]
-- >>> squareTree example
-- Cons (V 16) (Cons (Cons (V 9) (Cons (V 16) Nil)) (Cons (V 4) Nil))
-- >>> squareTree' example
-- Cons (V 16) (Cons (Cons (V 9) (Cons (V 16) Nil)) (Cons (V 4) Nil))
