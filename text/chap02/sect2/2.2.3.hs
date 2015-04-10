module SequencesasConventionalInterfaces where
import Data.List

-- restricting our interest for lists as in haskell:

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate seq_
    | null seq_ = []
    | predicate (head seq_) = head seq_ : filter' predicate (tail seq_)
    | otherwise = filter predicate (tail seq_)

-- |
-- >>> filter' odd [1,2,3,4,5]
-- [1,3,5]

accumulate :: (a -> b -> b) -> b -> [a] -> b
accumulate op initial seq_ =
        if null seq_
            then initial
            else op (head seq_) (accumulate op initial (tail seq_))

-- |
-- >>> accumulate (+) 0 [1..5]
-- 15
-- >>> accumulate (*) 1 [1..5]
-- 120
-- >>> accumulate (:) [] [1..5]
-- [1,2,3,4,5]

evenFibs :: Int -> [Int]
evenFibs = foldr (:) [] . filter even . map fib . enumFromTo 0
-- |
-- >>> evenFibs 10
-- [0,2,8,34]

listFibSquares :: Int -> [Int]
listFibSquares = foldr (:) [] . map (square . fib) . enumFromTo 0
-- lFS = foldr ((:) . square . fib) [] . enumFromTo 0
-- |
-- >>> listFibSquares 10
-- [0,1,1,4,9,25,64,169,441,1156,3025]

productOfSquaresOfOddElements :: [Int] -> Int
productOfSquaresOfOddElements = foldl (*) 1 . map square . filter odd
-- |
-- >>> productOfSquaresOfOddElements [1..5]
-- 225

salaryOfHighestPaidProgrammer :: [Person] -> Double
salaryOfHighestPaidProgrammer = foldl' max 0 . map salary . filter isProgrammer



fib :: Int -> Int
fib = let
    iter a b n = if n == 0
                     then b
                     else iter (a+b) a (n-1)
        in
            iter 1 0

square :: Num a => a -> a
square n = n*n

data Person = Person
isProgrammer :: Person -> Bool
isProgrammer = undefined
salary :: Person -> Double
salary = undefined
