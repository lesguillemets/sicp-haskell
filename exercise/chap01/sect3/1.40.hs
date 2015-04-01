module OneForty where
import NewtonsMethod (newtonsMethod)
cubic :: Double -> Double -> Double -> Double -> Double
cubic a b c x = x^3 + a*x^2 + b*x + c

-- | finds a root.
-- prop> let f = cubic (a::Double) (b::Double) (c::Double) in (abs $  f (newtonsMethod f 1.0)) < 0.00001
