-- | linear recursive process.
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

-- | linear iterative process.
-- | Nice optimizations take place with -O,
-- | but with -O0, this should also be linear recursive.
-- | Use bangpatterns to avoid this.
fact' :: Integer -> Integer
fact' = factIter 1

factIter :: Integer -> Integer -> Integer
factIter acc 1 = acc
factIter acc n = factIter (acc*n) (n-1)

main = do
    putStrLn . take 6 . show $ fact 90000
    putStrLn . take 6 . show $ fact' 90000
