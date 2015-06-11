import BTSet
import Data.Foldable (foldMap, Foldable)
import qualified Data.Foldable as F
import Data.Monoid

main = do
    let a = foldl (flip adjoinSet) EmptyTree ([3,4,2,7,5] :: [Int])
    let b = foldl (flip adjoinSet) EmptyTree ([8,5,4,27,15] :: [Int])
    print . toList $ unionSets a b

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Tree e l r) = foldMap f l `mappend` f e
                                         `mappend` foldMap f r

unionSets :: Ord a => Tree a -> Tree a -> Tree a
unionSets t0 t1 = fromOrderedList $ joinOrdered (toList t0) (toList t1)

fromOrderedList :: Ord a => [a] -> Tree a
fromOrderedList es = fst $ partialTree es (length es)

partialTree :: [a] -> Int -> (Tree a, [a])
partialTree elms 0 = (EmptyTree, elms)
partialTree elms n =
        let leftSize = (n-1) `div` 2
            rightSize = n - (leftSize +1)
            (leftTree, thisEntry:rightElms) = partialTree elms leftSize
            (rightTree,remainingElms) = partialTree rightElms rightSize
            in
                (Tree thisEntry leftTree rightTree, remainingElms)

toList :: Tree a -> [a]
toList = F.foldr (:) []

joinOrdered :: Ord a => [a] -> [a] -> [a]
joinOrdered [] xs = xs
joinOrdered xs [] = xs
joinOrdered l0@(x:xs) l1@(y:ys) =
        case x `compare` y of
            EQ -> y:joinOrdered xs ys
            GT -> y:joinOrdered l0 ys
            LT -> x:joinOrdered xs l1
