module Haffman where
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef
-- $setup
-- >>> import Data.List

frequencies :: Ord a => [a] -> M.Map a Int
frequencies xs = runST $ do
    m <- newSTRef M.empty
    forM_ xs $ \c -> do
        val <- M.lookup c <$> readSTRef m
        case val of
            (Just _) -> modifySTRef' m (M.adjust succ c)
            Nothing -> modifySTRef' m (M.insert c 1)
    readSTRef m

-- |
-- prop> (M.toAscList . frequencies) xs == (map (liftM2 (,) head length) . group . sort) (xs :: [Char])

