module OneTen where

ack _ 0 = 0
ack 0 y = 2 * y
ack _ 1 = 2
ack x y = ack (x-1) (ack x (y-1))

f = ack 0
g = ack 1
h = ack 2

-- | What are the values?
-- >>> ack 1 10
-- 1024
-- >>> ack 2 4
-- 65536
-- >>> ack 3 3
-- 65536
--
-- | ack 0 n = 2 * n
-- >>> all (\n -> f n == 2*n) [1..20]
-- True
--
-- | ack 1 n = ack 0 (ack 1 (n-1))
-- |  = 2*(ack 0 (ack 1 (n-2)))
-- |  ...
-- |  = 2*2*2* ... * (ack 1 1)
-- |  = 2^n
-- >>> all (\n -> g n == 2^n) [1..10]
-- True
--
-- | ack 2 n = ack 1 (ack 2 (n-1))
-- | = 2^(ack 2 (n-1))
-- | ack 2 1 = 2
-- >>> map h [1..5] == (take 5 $ iterate (2^) 2)
-- True

main = do
    print $ map f [1..5]
    print $ map g [1..5]
    print $ map h [1..4]
