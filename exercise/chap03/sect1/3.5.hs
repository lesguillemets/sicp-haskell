import System.Random
import Data.IORef
import MonteCarlo hiding (main)

estimateIntegral :: (Double -> Double -> Bool) -- tester
                 -> (Double,Double) -- (x0,x1) : xrange
                 -> (Double,Double) -- (y0,y1) : yrange
                 -> Int       -- trials
                 -> IO Double -- result
estimateIntegral checker xr@(x0,x1) yr@(y0,y1) trials = do
    g <- newIORef =<< getStdGen
    let tester = strunner g checker xr yr
        area = (x1-x0) * (y1-y0)
    (area *) <$> monteCarlo trials tester

strunner :: IORef StdGen
         -> (Double -> Double -> Bool)
         -> (Double,Double) -> (Double,Double)
         -> IO Bool
strunner g f xr yr = do
    x <- readRandR g xr
    y <- readRandR g yr
    return $ f x y

main = estimateIntegral (\x y -> x^2 + y^2 <= 1) (-1,1) (-1, 1) 10000
    >>= print

readRandR :: Random a => IORef StdGen -> (a,a) -> IO a
readRandR gen range = do
    (s,g') <- randomR range <$> readIORef gen
    writeIORef gen g'
    return s
