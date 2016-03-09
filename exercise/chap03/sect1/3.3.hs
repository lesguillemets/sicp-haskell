module BasicAccount where
import Data.IORef
import Control.Monad

-- Using IO for simplicity. We should probably use STRef etc. instead.

type Password = String
passwordPre :: String -> Password
passwordPre = id
checkPassword :: Password -> String -> Bool
checkPassword = (==)
callThePolice = putStrLn "BEEEEEEEEEEEEEP!"

makeAccount :: Int   -- balance
            -> String -- password
            -> IO (String -- command
                -> String -- password
                -> Int    -- amount
                -> IO (Either String Int))
makeAccount n pass = do
    balance <- newIORef n
    attempts <- newIORef 0
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
        savedPass = passwordPre pass
        dispatch m p a =
            if checkPassword savedPass p
                then do
                    writeIORef attempts 0
                    case m of
                         "withdraw" -> withdraw a
                         "deposit" -> deposit a
                         _ -> error "Unknown command"
                else do
                    modifyIORef' attempts succ
                    times <- readIORef attempts
                    when (times >= 7) callThePolice
                    return $ Left "Invalid Password"
        in return dispatch

-- |
-- >>> acc <- makeAccount 100 "foobar"
-- >>> acc "withdraw" "foobar" 50
-- Right 50
-- >>> acc "withdraw" "bazbaz" 50
-- Left "Invalid Password"
