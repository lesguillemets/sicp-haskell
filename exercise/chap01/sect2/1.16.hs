module OneSixteen where

fastExpt :: (Num a) => a -> Int -> a
fastExpt b n
    | n == 0 = 1
    | even n = (fastExpt b (n `div` 2) )^2
    | otherwise = b * fastExpt b (n-1)

fastExpt' :: (Num a) => a -> Int -> a
fastExpt' = fastExptIter 1
fastExptIter :: (Num a) => a -> a ->  Int -> a
fastExptIter a b n
    | n == 0 = a
    | even n = fastExptIter a (b*b) (n`div`2)
    | otherwise = fastExptIter (a*b) b (n-1)
main = putStrLn . take 6 . show . fastExpt 3 $ 90000000
