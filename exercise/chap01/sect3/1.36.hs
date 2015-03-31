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
            
fixedPointAveDamp :: (Show a, Num a, Ord a, Fractional a)
    => a -> (a -> a) -> a -> Writer [String] a
fixedPointAveDamp tore f firstGuess =
        let closeEnough = closerThan tore
            try guess = let next = (guess + f guess) / 2 in do
                tell ["Trying with " ++ show guess]
                if closeEnough guess next then return next
                                          else try next
        in
            try firstGuess

main = do
    let without = runWriter (fixedPoint torelance (`logBase` 1000) 3.0)
        with = runWriter (fixedPointAveDamp torelance (`logBase` 1000) 3.0)
    putStrLn . concat $ ["Without Average Damping: took ",
                        (show . length . snd) without,
                        " steps"
                        ]
    mapM_ putStrLn $ snd without
    putStrLn . concat $ ["With Average Damping: took ",
                        (show . length . snd) with,
                        " steps"
                        ]
    mapM_ putStrLn $ snd with

