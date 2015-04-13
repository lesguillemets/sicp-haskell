module TwoThirtyseven where

-- requires ExistentialQuantification and unconventional.
-- data Vector a = (Show a, Num a) =>  Vect [a]
data Vector a = Vector [a] deriving (Show, Eq)
data Matrix a = Matrix [[a]] deriving (Show, Eq)
v0 :: Vector Int
v0 = Vector [3,0,-2,3]
m0 :: Matrix Int
m0 = Matrix [[1,2,3,4],[4,5,6,6],[6,7,8,9]]

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct (Vector xs) (Vector ys) = sum $ zipWith (*) xs ys
-- |
-- >>> dotProduct (Vector [2,3,4]) (Vector [-2,4,3])
-- 20

mTimesV :: (Num a) => Matrix a -> Vector a -> Vector a
mTimesV (Matrix m) (Vector xs) = Vector $ map (sum . zipWith (*) xs) m
-- |
-- >>> m0 `mTimesV` v0
-- Vector [9,18,29]

accMap :: (a -> b -> b) -> b -> [[a]] -> [b]
accMap f ini seqs = if null (head seqs)
                        then []
                        else
                            foldr (f . head) ini seqs :
                                accMap f ini (map tail seqs)

transpose :: Matrix a -> Matrix a
transpose (Matrix es) = Matrix (accMap (:) [] es)
-- |
-- >>> transpose m0
-- Matrix [[1,4,6],[2,5,7],[3,6,8],[4,6,9]]

mTimesM :: (Num a) => Matrix a -> Matrix a ->  Matrix a
mTimesM (Matrix es0) mat1 =
        let (Matrix es') = transpose mat1
            in
                Matrix $ map (\l -> map (sum . zipWith (*) l) es') es0
ma = Matrix [[2,3],[4,5]]
mb = Matrix [[1,0],[2,3]]
e = Matrix [[1,0],[0,1]]
-- |
-- >>> ma `mTimesM` mb
-- Matrix [[8,9],[14,15]]
-- >>> mb `mTimesM` ma
-- Matrix [[2,3],[16,21]]
-- >>> ma `mTimesM` e == ma && e `mTimesM` ma == ma
-- True
