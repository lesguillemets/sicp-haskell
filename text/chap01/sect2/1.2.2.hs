module OneTwoTwo where

-- | simple, but slow.
-- >>> fib 10
-- 55
fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | much faster.
-- >>> fib' 30
-- 832040
fib' :: Integral a => a -> a
fib' = fibIter 1 0
fibIter _ b 0 = b
fibIter a b count = fibIter (a+b) a (count -1)

-- | This works.
-- >>> countChange 100
-- 292
countChange :: Int -> Int
countChange amount = cc amount 5

cc :: Int -> Int -> Int
cc amount kindsOfCoins
    | amount == 0 = 1
    | amount < 0 || kindsOfCoins == 0 = 0
    | otherwise = cc amount (kindsOfCoins -1) + -- pay with no more of this
                    cc (amount - firstDenomination kindsOfCoins) kindsOfCoins
                    -- or use one coin of this kind and continue

firstDenomination :: Int -> Int
firstDenomination kindsOfCoins = [1,5,10,25,50] !! (kindsOfCoins-1)

-- | I'd use cc :: Int -> [Coin] -> Int, though.
-- >>> countChange' 100
-- 292
type Coin = Int
countChange' :: Int -> Int
countChange' amount = cc' amount coins
cc' :: Int -> [Coin] -> Int
cc' _ [] = 0
cc' amount (cns@(c:cs))
    | amount < 0 = 0 -- necessary.
    | amount == 0 = 1
    | otherwise = cc' amount cs + cc' (amount - c) cns

coins :: [Coin]
coins = [50, 25, 10, 5, 1]
