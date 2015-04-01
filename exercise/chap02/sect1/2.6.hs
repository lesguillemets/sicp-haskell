module TwoSix where

zero = const id
addOne n = \f -> f . n f
-- zero = \f -> (\x -> x)
-- addOne n = \f -> (\x -> f ((n f) x))

-- one = addOne zero
--  = \f -> f . zero f = \f -> f . id = \f -> f = (id :: (a -> b) -> a -> b))
one' :: (a -> b) -> (a -> b)
one' = id

-- two = addOne one
--     = \f -> f . id f = \f -> f . f
two' = \f -> f . f

-- such beauty ...
-- OK, then..
add n m = \f -> n f . m f

one = addOne zero
two = addOne one
three = add one two
four = add two two
-- |
-- >>> one succ $ 0
-- 1
-- >>> two succ $ 0
-- 2
-- >>> two' succ $ 0
-- 2
-- >>> three succ $ 0
-- 3
-- >>> four succ $ 0
-- 4
