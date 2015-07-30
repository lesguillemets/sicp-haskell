module Complex where
import Data.Fixed (mod')
-- we can use pattern-match as tags,
-- so while keeping in mind that additional "tag"s can be used for
-- implementation, we'll make use of haskell's data declaration.

data Complex a = Rect {_real :: a, _imag :: a}
               | Polar { _r :: a, _arg :: a} deriving (Show)

real :: Floating a => Complex a -> a
real (Rect x _) = x
real (Polar r θ) = r * cos θ

imag :: Floating a => Complex a -> a
imag (Rect _ y) = y
imag (Polar r θ) = r * sin θ

arg :: Floating a => Complex a -> a
arg (Rect x y) = atan (y/x)
arg (Polar _ θ) = θ

mag :: Floating a => Complex a -> a
mag (Rect x y) = sqrt $ x^(2::Int) + y^(2::Int)
mag (Polar r _) = r

toRect :: Floating a => Complex a -> Complex a
toRect z = Rect (real z) (imag z)

toPolar :: Floating a => Complex a -> Complex a
toPolar z = Polar (mag z) (arg z)

-- use with caution, because fmap f z results in totally different results
-- depending on the represantation (Polar or Rect).
instance Functor Complex where
    fmap f (Rect x y) = Rect (f x) (f y)
    fmap f (Polar r θ) = Rect (f r) (f θ)

instance Floating a => Num (Complex a) where
    z0 + z1 = Rect (real z0 + real z1) (imag z0 + imag z1)
    z0 * z1 = Polar (mag z0 * mag z1) (arg z0 + arg z1)
    abs = fmap abs . toRect
    signum = fmap signum
    fromInteger n = Rect (fromInteger n) 0

