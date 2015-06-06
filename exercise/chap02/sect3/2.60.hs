module TwoSixty where

newtype LSet a = LSet [a] deriving (Show)
cons :: a -> LSet a -> LSet a
cons x (LSet xs) = LSet (x:xs)

elemOfSet :: (Eq a) => a -> LSet a -> Bool
elemOfSet _ (LSet []) = False
elemOfSet el (LSet (x:xs)) = (x == el) || elemOfSet el (LSet xs)

adjoinSet :: a -> LSet a -> LSet a
adjoinSet = cons

unionSet :: LSet a -> LSet a -> LSet a
unionSet (LSet l0) (LSet l1) = LSet (l0++l1)

intersectionSet :: (Eq a) => LSet a -> LSet a -> LSet a
intersectionSet (LSet []) _ = LSet []
intersectionSet _ (LSet []) = LSet []
intersectionSet (LSet (x:xs)) s =
        (if x `elemOfSet` s
            then cons x
            else id) $ intersectionSet (LSet xs) s
