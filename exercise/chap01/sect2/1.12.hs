module OneTwelve where

-- Write a procedure that computes elements of Pascal's triangle by means
-- of a recursive process.
-- Not sure what the expected input is..

pascalAt :: Int -> Int -> Int
pascalAt row column
    | column == 0 || column == row = 1
    | column < 0 || row < column = 0
    | otherwise = pascalAt (row-1) column + pascalAt (row-1) (column-1)

-- | Let's check, anyway.
-- >>> pascalAt 0 0
-- 1
-- >>> pascalAt 2 1
-- 2
-- >>> map (pascalAt 3) [0..3]
-- [1,3,3,1]
-- >>> map (pascalAt 4) [0..4]
-- [1,4,6,4,1]
