{-# LANGUAGE BangPatterns #-}
contFrac :: Fractional a => (Int -> a) -> (Int -> a) -> Int -> a
contFrac di ni n =
        let iter k = if k == n
                       then ni k / di k
                       else ni k / (di k + iter (k+1))
        in
            iter 0

contFrac' :: Fractional a => (Int -> a) -> (Int -> a) -> Int -> a
contFrac' di ni n =
        let iter !acc k = if k == 0
                             then acc
                             else iter (ni k / (di k + acc)) (k-1)
        in
            iter (ni n / di n) (n-1)

main = mapM_ (print . contFrac' (const 1.0) (const 1.0)) $ [5..20]
