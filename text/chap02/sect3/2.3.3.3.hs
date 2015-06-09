{-# LANGUAGE RankNTypes #-}
module BTSet where

data Tree a = Tree { entry :: a, lBranch :: Tree a, rBranch :: Tree a }
            | EmptyTree
instance Show a => Show (Tree a) where
    show EmptyTree = "."
    show (Tree e l r) = concat [ show e, "(",
                                show l, ",", show r, ")" ]

type BTSet a = Ord a => Tree a

elemOfSet :: Ord a => a -> Tree a -> Bool
elemOfSet _ EmptyTree = False
elemOfSet x (Tree e l r) =
        case x `compare`  e of
            EQ -> True
            LT -> elemOfSet x l
            GT -> elemOfSet x r

adjoinSet :: Ord a => a -> Tree a -> Tree a
adjoinSet x EmptyTree = Tree x EmptyTree EmptyTree
adjoinSet x t@(Tree e l r) =
        case x `compare` e of
            EQ -> t
            LT -> Tree e (adjoinSet x l) r
            GT -> Tree e l (adjoinSet x r)
-- |
-- >>> import Data.List
-- >>> mapM_ print $ scanl (flip adjoinSet) EmptyTree [3,4,2,8,2]
-- .
-- 3(.,.)
-- 3(.,4(.,.))
-- 3(2(.,.),4(.,.))
-- 3(2(.,.),4(.,8(.,.)))
-- 3(2(.,.),4(.,8(.,.)))
