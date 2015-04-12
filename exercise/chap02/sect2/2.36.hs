module TwoThirtysix where
import Data.List (transpose)

-- sensing more generalisations
accMap :: (a -> b -> b) -> b -> [[a]] -> [b]
accMap f ini seqs = if null (head seqs)
                        then []
                        else
                            foldr (f . head) ini seqs :
                                accMap f ini (map tail seqs)

-- Of course we can use this...
accMap' :: (a -> b -> b) -> b -> [[a]] -> [b]
accMap' f ini = map (foldr f ini) . transpose

-- |
-- >>> accMap (+) 0 [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
-- [22,26,30]
--
-- >>> accMap' (+) 0 [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
-- [22,26,30]
