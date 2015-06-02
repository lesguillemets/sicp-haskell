module ExSets where

newtype LSet a = LSet [a]
cons :: a -> LSet a -> LSet a
cons x (LSet xs) = LSet (x:xs)

elemOfSet :: (Eq a) => a -> LSet a -> Bool
elemOfSet _ (LSet []) = False
elemOfSet el (LSet (x:xs)) = (x == el) || elemOfSet el (LSet xs)

adjoinSet :: (Eq a) => a -> LSet a -> LSet a
adjoinSet x se = if x `elemOfSet` se then se
                                     else cons x se

intersectionSet :: (Eq a) => LSet a -> LSet a -> LSet a
intersectionSet (LSet []) _ = LSet []
intersectionSet _ (LSet []) = LSet []
intersectionSet (LSet (x:xs)) s1 =
        ( if x `elemOfSet` s1
            then cons x
            else id ) $ intersectionSet (LSet xs) s1
