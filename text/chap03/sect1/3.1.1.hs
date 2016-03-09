module BasicAccount where
import Data.IORef

-- Using IO for simplicity. We should probably use STRef etc. instead.
makeWithDraw :: Int -> IO (Int -> IO (Either String Int))
makeWithDraw n = do
    balance <- newIORef n
    return $ \amount -> do
        bal <- readIORef balance
        if bal >= amount
           then do
               modifyIORef' balance (subtract amount)
               Right <$> readIORef balance
           else return $ Left "Insufficient funds"

-- |
-- >>> wd <- makeWithDraw 100
-- >>> wd' <- makeWithDraw 20
-- >>> wd 10
-- Right 90
-- >>> wd' 40
-- Left "Insufficient funds"
-- >>> wd 10
-- Right 80
-- >>> wd 90
-- Left "Insufficient funds"

makeAccount :: Int -> IO (String -> Int -> IO (Either String Int))
makeAccount n = do
    balance <- newIORef n
    let withdraw amount = do
            b <- readIORef balance
            if b >= amount
                then do
                    modifyIORef' balance (subtract amount)
                    Right <$> readIORef balance
                else return $ Left "Insufficient funds"
        deposit amount = do
            modifyIORef' balance (+ amount)
            Right <$> readIORef balance
        dispatch m = case m of
                          "withdraw" -> withdraw
                          "deposit" -> deposit
                          _ -> error "Unknown command"
                          -- FIXME : error handling
        in return dispatch

-- |
-- >>> acc <- makeAccount 100
-- >>> acc "withdraw" 50
-- Right 50
-- >>> acc "withdraw" 60
-- Left "Insufficient funds"
-- >>> acc "deposit" 40
-- Right 90
-- >>> acc "withdraw" 60
-- Right 30
