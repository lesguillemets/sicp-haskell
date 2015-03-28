import Timer

main = do
    let searchPrimeSq = searchPrimeWith isPrime 3 :: Integer -> [Integer]
    mapM (timeIt "done" . searchPrimeSq) (take 10 $ iterate (*10) 100)
        >>= mapM_ print

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

square :: (Num a) => a -> a
square = (^2)
doesDivide :: (Integral a) => a -> a -> Bool
doesDivide = ((== 0) . ) . flip mod
