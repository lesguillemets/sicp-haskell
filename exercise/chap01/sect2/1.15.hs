import Control.Monad.Writer

-- | Let's check if the following sine works.
-- >>> import Text.Printf (printf)
-- >>> printf "%.3f" . fst . runWriter $ sine (pi/6)
-- 0.500
--
-- | Check for more values!
-- >>> let rads = [0.1, 0.2 .. 3.1]
-- >>> let ss = map sin rads
-- >>> let ss' = map (fst . runWriter . sine) rads
-- >>> all ( (< 0.005) . abs) $ zipWith (-) ss ss'
-- True
--
-- | OK, then how many times is the procedure p applied?
-- >>> snd . runWriter $ sine 12.15
-- 5
--
-- | Let's see how p varies as n x progresses..
sine :: Double -> Writer Count Double
sine angle = if abs angle <= 0.1 then return angle
                                 else do
                                     result <- sine (angle/3)
                                     tell 1
                                     return (p result)

p :: Double -> Double
p x = 3 * x - 4 * cube x

cube :: Double -> Double
cube = (^3)
type Count = Int

instance Monoid Int where
    mempty = 0
    mappend = (+)

main = do
    print $ runWriter (sine 12.15)
    print $ runWriter (sine (pi/3))
    let rads = [0.1, 0.2 .. 3.1]
        s =  map (fst . runWriter . sine) rads
        s' = map sin rads
    print $ zipWith (-) s' s
