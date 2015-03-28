module Timer where
import Data.Time

timeIt :: String -> a -> IO Double
timeIt mes a = do
    time0 <- getCurrentTime
    let b = seq a ""
    putStr b
    time1 <- getCurrentTime
    putStrLn mes
    return $ realToFrac (diffUTCTime time1 time0)
