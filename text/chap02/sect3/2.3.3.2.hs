module OLSet where
-- $setup
-- >>> let a = OLSet ([1,2,3,6,7,8] :: [Int])
-- >>> let b = OLSet ([1,2,5,6,10,12] :: [Int])

newtype OLSet a = OLSet [a] deriving (Show, Eq)
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
        LT -> False
        GT -> elemOfSet e (OLSet xs)
-- |
-- >>> elemOfSet 1 a
-- True
-- >>> elemOfSet 10 a
-- False

intersectionSet :: Ord a => OLSet a -> OLSet a -> OLSet a
intersectionSet (OLSet []) _ = OLSet []
intersectionSet _ (OLSet []) = OLSet []
intersectionSet s0@(OLSet (x0:xs0)) s1@(OLSet (x1:xs1)) =
    case x0 `compare` x1 of
        EQ -> cons x1 $ intersectionSet (OLSet xs0) (OLSet xs1)
        LT -> intersectionSet (OLSet xs0) s1
        GT -> intersectionSet s0 (OLSet xs1)
-- |
-- >>> intersectionSet a b
-- OLSet [1,2,6]

-- 2.61
adjoinSet :: Ord a => a -> OLSet a -> OLSet a
adjoinSet x (OLSet sl) = OLSet $ let
    (pre,post) = span (< x) sl in
        case post of
            [] -> pre ++ [x]
            (h:rest) -> if x == h then sl
                                  else pre ++ x:post

-- |
-- >>> adjoinSet 4 a
-- OLSet [1,2,3,4,6,7,8]
--
-- >>> adjoinSet 2 a
-- OLSet [1,2,3,6,7,8]
--
-- >>> adjoinSet 10 a
-- OLSet [1,2,3,6,7,8,10]

-- 2.62
unionSet :: (Ord a) => OLSet a -> OLSet a -> OLSet a
unionSet (OLSet s0) (OLSet s1) = OLSet $ union s0 s1
    where
        union [] ys = ys
        union xs [] = xs
        union l0@(x:xs) l1@(y:ys) =
            case x `compare` y of
                EQ -> x : union xs ys
                GT -> y : union l0 ys
                LT -> x : union xs l1
-- |
-- >>> unionSet a b
-- OLSet [1,2,3,5,6,7,8,10,12]
