module TwoFortytwo where

-- n-queen
type Position = (Int,Int)
queens :: Int -> [[Position]]

queens n = queenCols n where
    queenCols :: Int -> [[Position]]
    queenCols 0 = [[]]
    queenCols k =
        concatMap (\q -> map (q:) $ filter (safetoAdd q) (queenCols (k-1)))
                    [(k,i) | i <- [1..n]]
-- |
-- >>> map (length . queens) [1..6]
-- [1,0,0,2,10,4]

safetoAdd :: Position -> [Position] -> Bool
safetoAdd q = not . any (checks q)

checks :: Position -> Position -> Bool
checks (x0,y0) (x1,y1) = x0 == x1 || y0 == y1
                         || (x0+y0) == (x1+y1) || (x0-y0) == (x1-y1)
