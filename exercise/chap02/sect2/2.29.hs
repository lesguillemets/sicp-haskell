module TwoNineteen where
import TList

makeMobile :: TList a -> TList a -> TList a
makeMobile left right = list [left,right]

leftBranch :: TList a -> TList a
leftBranch = car
rightBranch :: TList a -> TList a
rightBranch = car . cdr

branchLength :: TList a -> TList a
branchLength = car
branchStructure :: TList a -> TList a
branchStructure = car . cdr

makeBranch :: Int -> TList Int -> TList Int
makeBranch len structure = list [V len, structure]

totalWeight :: TList Int -> Int
totalWeight Nil = 0
totalWeight (V _) = 0
totalWeight (Cons (V _) (Cons (V weight) Nil)) = weight
totalWeight mob = totalWeight (rightBranch $ mob)
                    + totalWeight (leftBranch $ mob)

isBalanced :: TList Int -> Bool
isBalanced x = totalWeight (rightBranch x) == totalWeight (leftBranch x)
-- |
-- >>> let lev0 = makeMobile (makeBranch 3 (V 5)) (makeBranch 2 (V 10))
-- >>> let lev1 = makeMobile (makeBranch 5 lev0) (makeBranch 4 (V 4))
-- >>> let leftHand = makeBranch 20 (V 19)
-- >>> let rightHand = makeBranch 15 lev1
-- >>> let lev2 = makeMobile leftHand rightHand
-- >>> totalWeight lev0
-- 15
-- >>> totalWeight lev1
-- 19
-- >>> isBalanced lev1
-- False
-- >>> totalWeight lev2
-- 38
-- >>> isBalanced lev2
-- True
