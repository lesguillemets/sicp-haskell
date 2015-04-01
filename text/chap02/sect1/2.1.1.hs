module ArithmeticRats where

data Rat' =  Rat' {numer' :: Int,  denom' :: Int} deriving (Show)

instance Eq Rat' where
    x == y = numer' x * denom' y == numer' y * denom' x

instance Num Rat' where
    x + y = Rat' n d where
        n = numer' x * denom' y + numer' y * denom' x
        d = denom' x * denom' y
    x * y = Rat' n d where
        n = numer' x * numer' y
        d = denom' x * denom' y
    negate (Rat' n d) = Rat' (-n) d
    abs (Rat' n d) = Rat' (abs n) (abs d)
    fromInteger n = Rat' (fromIntegral n) 1
    signum (Rat' x y) = fromIntegral . signum $ x*y

-- This also solves ex 2.1.
mkRat' :: Int -> Int -> Rat'
mkRat' n d = let g = gcd n d
                 sign = signum d
                 in
                     Rat' (sign * n `div` g) (sign * d `div` g)

-- cons in LISP seems to correspond to (,) (rather than (:)) in haskell.
-- we can probably use newtype
type Rat = (Int,Int)
cons = (,)
mkRat = cons
numer = fst
denom = snd

showRat :: Rat -> String
showRat x = (show . numer) x ++ " / " ++ (show . denom) x
-- the rest is the same as above.
