{-# LANGUAGE ScopedTypeVariables #-}
module MonteCarlo where
import System.Random
import Data.IORef

-- |
-- >>> (< 0.01) . abs . subtract pi <$> estimatePi 100000
-- True

estimatePi :: Int -> IO Double
estimatePi trials = do
    g <- newIORef =<< getStdGen
    sqrt . (6/) <$> monteCarlo trials (cesaroTest g)

cesaroTest :: IORef StdGen -> IO Bool
cesaroTest g = do
    n <- abs <$> readRand g
    m <- abs <$> readRand g
    return $ gcd n m == (1 :: Integer)

monteCarlo :: Int -> IO Bool -> IO Double
monteCarlo trials experiment =
    let iter 0 trialsPassed =
            return $ fromIntegral trialsPassed / fromIntegral trials
        iter trialsRemaining trialsPassed = do
            t <- experiment
            iter (pred trialsRemaining) $
                if t then succ trialsPassed else trialsPassed
        in
        iter trials 0

main = estimatePi 100000 >>= print

readRand :: Random a => IORef StdGen -> IO a
readRand gen = do
    (s,g') <- random <$> readIORef gen
    writeIORef gen g'
    return s
-- fmm... perhaps simulating the concept using IORef isn't a good idea.
