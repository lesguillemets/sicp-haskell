module TwoThirtyfive where
data Tree a = Value a | Branch [Tree a] deriving (Show)

-- wait, don't we need to flatten the list anyway?
countLeaves :: Tree a -> Int
countLeaves (Value _) = 1
countLeaves (Branch ts) = foldr ((+) . countLeaves) 0 ts
-- |
-- >>> countLeaves t
-- 6

t = Branch [
           Value 3,
           Branch [Value 3.3, Value 2.4],
           Branch [
                  Value 3,
                  Branch [
                         Value 4,
                         Value 1
                         ]
                ]
           ]
