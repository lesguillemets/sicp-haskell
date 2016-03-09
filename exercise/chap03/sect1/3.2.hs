{-# LANGUAGE TupleSections #-}
module MakeMonitored where
import Data.IORef

-- this is the best I could think of..

makeMonitored :: (a -> b) -> IO (a -> IO (b,Int))
makeMonitored f = do
    counter <- newIORef 0
    return $ \x -> do
        modifyIORef' counter succ
        (f x,) <$> readIORef counter

-- |
-- >>> s <- makeMonitored sqrt
-- >>> s 100
-- (10.0,1)
-- >>> s 100
-- (10.0,2)
