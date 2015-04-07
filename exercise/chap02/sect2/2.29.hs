module TwoNineteen where
import TList

makeMobile :: TList a -> TList a -> TList a
makeMobile left right = list [left,right]

leftBranch :: TList a -> TList a
leftBranch = car
rightBranch :: TList a -> TList a
rightBranch = car . cdr

totalWeight :: TList Int -> Int
totalWeight Nil = 0
totalWeight (V n) = n
totalWeight mob = totalWeight (rightBranch mob) + totalWeight (leftBranch mob)

isBalanced :: TList Int -> Bool
isBalanced x = totalWeight (rightBranch x) == totalWeight (leftBranch x)
-- need test cases.
