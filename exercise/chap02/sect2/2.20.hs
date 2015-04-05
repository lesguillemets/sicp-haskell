module TwoTwenty where
import SchemeList
-- different, but anyway
sameParity :: SchList Int -> SchList Int
sameParity ns = sameParity' (car ns) ns

sameParity' :: Int -> SchList Int -> SchList Int
sameParity' _ Nil = Nil
sameParity' n (Cons a as) = if n `mod` 2 == a `mod` 2
                               then Cons a (sameParity' n as)
                               else sameParity' n as

-- |
-- >>> sameParity (scL [1..7])
-- Cons 1 (Cons 3 (Cons 5 (Cons 7 Nil)))
--
-- >>> sameParity (scL [2..7])
-- Cons 2 (Cons 4 (Cons 6 Nil))
