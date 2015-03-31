module OneThirtyfive where

torelance :: Double
torelance = 0.00001
closerThan :: (Num a, Ord a) => a -> a -> a -> Bool
closerThan tor v0 v1 = abs (v1 - v0) < tor

fixedPoint :: (Num a, Ord a) => a -> (a -> a) -> a -> a
fixedPoint tore f firstGuess =
        let closeEnough = closerThan tore
            try guess = let next = f guess in
                if closeEnough guess next then next else try next
        in
            try firstGuess

-- | 1 + 2/(1+sqrt 5) = 1 + (sqrt 5 -1)/2 = (1 + sqrt 5) / 2
-- >>> import Text.Printf
-- >>> printf "%.5f" $ fixedPoint torelance (\x->1+1/x) 1.0
-- 1.61803
