module OneThirtyfive where
import Control.Monad.Writer

torelance :: Double
torelance = 0.00001
closerThan :: (Num a, Ord a) => a -> a -> a -> Bool
closerThan tor v0 v1 = abs (v1 - v0) < tor

-- How should I keep logs?
fixedPoint :: (Show a, Num a, Ord a)
    => a -> (a -> a) -> a -> Writer [String] a
fixedPoint tore f firstGuess =
        let closeEnough = closerThan tore
            try guess = let next = f guess in do
                tell ["Trying with " ++ show guess]
                if closeEnough guess next then return next
                                          else try next
        in
            try firstGuess

main = mapM_ print . snd $
    runWriter (fixedPoint torelance (`logBase` 1000) 3.0)
