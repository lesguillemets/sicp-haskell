import Timer

main = do
    let
        searchPrime = searchPrimeWith isPrime 3 :: Integer -> [Integer]
        searchPrime' = searchPrimeWith isPrime' 3 :: Integer -> [Integer]
        nums = take 10 $ iterate (*10) 100
    primes <- mapM (timeIt "Trying the original" . searchPrime) nums
    primes' <- mapM (timeIt "Trying the better" . searchPrime') nums
    mapM_ print $ zip primes primes'
    putStrLn " ratios : "
    mapM_ print $ zipWith (/) primes primes'

searchPrimeWith :: (Integral a) => (a -> Bool) -> Int -> a -> [a]
searchPrimeWith f n lowerBound = take n . filter f $ [lowerBound..]

isPrime :: (Integral a) => a -> Bool
isPrime n = n == smallestDivisor n
smallestDivisor :: (Integral a) => a -> a
smallestDivisor = flip findDivisor 2

findDivisor :: (Integral a) => a -> a -> a
findDivisor n testDiv
    | square testDiv > n = n
    | testDiv `doesDivide` n = testDiv
    | otherwise = findDivisor n (testDiv + 1)

isPrime' :: (Integral a) => a -> Bool
isPrime' n = n == smallestDivisor' n
smallestDivisor' :: (Integral a) => a -> a
smallestDivisor' = flip findDivisor' 2

findDivisor' :: (Integral a) => a -> a -> a
findDivisor' n testDiv
    | square testDiv > n = n
    | testDiv `doesDivide` n = testDiv
    | otherwise = findDivisor' n (next testDiv)

square :: (Num a) => a -> a
square = (^2)
doesDivide :: (Integral a) => a -> a -> Bool
doesDivide = ((== 0) . ) . flip mod

next :: Integral a => a -> a
next 2 = 3
next n = n + 2
