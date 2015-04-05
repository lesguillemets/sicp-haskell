module TwoTwentyone where

-- OK I've given up using SchemeList ..
squareList :: Num a => [a] -> [a]
squareList [] = []
squareList xs = (:) ((^2) . head $ xs) (squareList $ tail xs)
-- |
-- >>> squareList [1..5]
-- [1,4,9,16,25]

squareList' = map (^2)
-- |
-- >>> squareList' [1..5]
-- [1,4,9,16,25]
