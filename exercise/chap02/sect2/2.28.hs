module TwoTwentyeight where

import TList

fringe :: TList a -> [a]
fringe Nil = []
fringe (V a) = [a]
fringe (Cons p q) = fringe p ++ fringe q

-- |
-- >>> let x = list [fromList [1,2], fromList [3,4]]
-- >>> fringe x
-- [1,2,3,4]
--
-- >>> fringe $ list [x,x]
-- [1,2,3,4,1,2,3,4]
