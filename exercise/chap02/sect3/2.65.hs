import BTSet
import Data.Foldable (foldMap, Foldable)
import qualified Data.Foldable as F
import Data.Monoid

main = do
    let a = foldl (flip adjoinSet) EmptyTree [3,4,2,7,5]
    let b = foldl (flip adjoinSet) EmptyTree [8,5,4,27,15]
    print $ toList b

instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Tree e l r) = foldMap f l `mappend` f e
                                         `mappend` foldMap f r

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
