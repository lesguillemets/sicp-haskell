module OLSet where
newtype OLSet a = OLSet [a]
-- well, Data.Vector may be more suitable

cons :: a -> OLSet a -> OLSet a
cons x (OLSet xs) = OLSet (x:xs)
addElem :: Ord a => a -> OLSet a -> OLSet a
addElem x (OLSet xs) = let (pre,post) = span (< x) xs
                           in
                               OLSet $ pre ++ x:post

elemOfSet :: Ord a => a -> OLSet a -> Bool
elemOfSet _ (OLSet [])  = False
elemOfSet e (OLSet (x:xs)) =
    case e `compare` x of
        EQ -> True
        GT -> False
        LT -> elemOfSet e (OLSet xs)

intersectionSet :: Ord a => OLSet a -> OLSet a -> OLSet a
intersectionSet (OLSet []) _ = OLSet []
intersectionSet _ (OLSet []) = OLSet []
intersectionSet s0@(OLSet (x0:xs0)) s1@(OLSet (x1:xs1)) =
    case x0 `compare` x1 of
        EQ -> cons x1 $ intersectionSet (OLSet xs0) (OLSet xs1)
        LT -> intersectionSet (OLSet xs0) s1
        GT -> intersectionSet s0 (OLSet xs1)
