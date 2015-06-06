module ExSets where

newtype LSet a = LSet [a] deriving (Show)
cons :: a -> LSet a -> LSet a
cons x (LSet xs) = LSet (x:xs)

fromList :: (Eq a) => [a] -> LSet a
fromList [] = LSet []
fromList (x:xs) = adjoinSet x (fromList xs)

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

unionSet :: (Eq a) => LSet a -> LSet a -> LSet a
unionSet (LSet []) s = s
unionSet s (LSet []) = s
unionSet (LSet (x:xs)) s = adjoinSet x (unionSet (LSet xs) s)
