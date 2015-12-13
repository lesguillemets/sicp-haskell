{-# LANGUAGE ExistentialQuantification #-}
module SymbolicDifferentiation where

-- class Operator a where
--     derivRule :: a -> Equation -> Equation -> Equation
--
--
-- data Equation = Value Int
--               | Var String
--               | forall a. (Show a,Operator a) => Node a Equation Equation

class Show a => Differentiable a where
    deriv ::  a -> Variable -> Expression

data Expression = forall a. Differentiable a => Exp a
newtype Value = Value Int deriving (Eq)
newtype Variable = Var String deriving (Eq)
data Sum = forall a b. (Differentiable a, Differentiable b) => Sum a b
data Prod = forall a b. (Differentiable a, Differentiable b) => Prod a b

instance Differentiable Expression where
    deriv (Exp e) = deriv e

instance Differentiable Value where
    deriv _ _ = Exp $ Value 0

instance Differentiable Variable where
    deriv v x = if v == x then Exp $ Value 1
                          else Exp $ Value 0
instance Differentiable Sum where
    deriv (Sum a b) v = Exp $ Sum (deriv a v) (deriv b v)
    
instance Differentiable Prod where
    deriv (Prod a b) v = Exp $ Sum (Prod (deriv a v) b) (Prod a (deriv b v))

instance Show Value where
    show (Value n) = show n
instance Show Variable where
    show (Var v) = v
instance Show Expression where
    show (Exp a) = show a
instance Show Sum where
    show (Sum a b) = '(' : show a ++ " + " ++ show b ++ ")"
instance Show Prod where
    show (Prod a b) = '(' : show a ++ "*" ++ show b ++ ")"

-- $setup
-- >>> let x = Var "x"
-- >>> let y = Var "y"
-- >>> let eq0 = Exp $ Prod x y
-- >>> let eq1 = Exp $ Sum x eq0

-- |
-- >>> deriv (Exp x) x
-- 1
-- >>> deriv eq0 x
-- ((1*y) + (x*0))
-- >>> deriv eq1 x
-- (1 + ((1*y) + (x*0)))
-- >>> deriv eq1 y
-- (0 + ((0*y) + (x*1)))
