module TwoTwntyfive where

import TList

l0 = list [V 1, V 3, fromList [5,7], V 9]
-- | '(1 3 '(5 7) 0)
-- >>> unpack . car . cdr . car . cdr . cdr $ l0
-- 7

l1 = list [list [V 7]]
-- |
-- >>> unpack . car . car $ l1
-- 7

l2 = Cons (V 1) (Cons (V 2)
                    (Cons (V 3) (Cons (V 4) (Cons (V 5) (Cons (V 6)
                        (Cons (V 7) Nil))))))
-- | (1 (2 (3 (4 (5 (6 (7)))))))
-- >>> unpack . car . cdr . cdr . cdr . cdr . cdr . cdr $ l2
-- 7
