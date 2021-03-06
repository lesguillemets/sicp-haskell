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

---
-- Nested Mappings

pairsTo :: Int -> [(Int,Int)]
pairsTo = concatMap (\i -> map ((,) i) (enumFromTo 1 (i-1))) . enumFromTo 1
pT :: Int -> [(Int,Int)]
pT n = [(i,j) | j <- [1..n], i <- [j+1..n]]


-- flatMap is concatMap.

makePairSum :: Num a => (a,a) -> a
makePairSum = uncurry (+)

sumsToPrime :: (Int,Int) -> Bool
sumsToPrime = isPrime . makePairSum

primeSumPairs :: Int -> [(Int,Int)]
primeSumPairs = filter sumsToPrime . pairsTo
-- |
-- >>> sort . map makePairSum $ primeSumPairs 6
-- [3,5,5,7,7,7,11]

isPrime :: Int -> Bool
isPrime n = 0 `notElem` [n `mod` i | i <- [2..n-1]]

--
permutations' :: [a] -> [[a]]
permutations' = map (map snd) . makePermsNumbered . zip [0..]
makePermsNumbered :: [(Int,a)] -> [[(Int,a)]]
makePermsNumbered [] = [[]]
makePermsNumbered es = concatMap
    (\x -> map (x:) (makePermsNumbered (remove x es))) es
    where
        remove (n,_) = filter ((/= n) . fst)
-- |
-- >>> sort (permutations' [1,2,3]) == sort (permutations [1,2,3])
-- True
