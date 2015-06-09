module TwoSixtyThree where
import BTSet

fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip adjoinSet) EmptyTree

toList0 :: Tree a -> [a]
toList0 EmptyTree = []
toList0 (Tree e l r) = toList0 l ++ e:toList0 r

toList1 :: Tree a -> [a]
toList1 = flip copyToList [] where
    copyToList EmptyTree resl = resl
    copyToList (Tree e l r) resl =
        copyToList l (e: copyToList r resl)

-- |
-- prop> let t = fromList (xs::[Int]) in toList0 t == toList1 t
