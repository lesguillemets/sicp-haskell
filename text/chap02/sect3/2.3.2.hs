module SymbolicDifferentiation where

newtype Var = Var String deriving (Eq)
instance Show Var where
    show (Var s) = '\'' : s


data Equation a =
        Value a
        | Variable Var
        | Sum { augend :: Equation a, addend :: Equation a }
        | Product { multiplier :: Equation a, multiplicand :: Equation a }

mkSum :: (Eq a, Num a) => Equation a -> Equation a -> Equation a
mkSum (Value 0) adnd = adnd
mkSum aug (Value 0) = aug
mkSum (Value x) (Value y) = Value (x+y)
mkSum aug adnd = Sum aug adnd

mkProd :: (Eq a, Num a) => Equation a -> Equation a -> Equation a
mkProd (Value 0) _ = Value 0
mkProd _ (Value 0) = Value 0
mkProd (Value 1) mlpc = mlpc
mkProd mlpr (Value 1) = mlpr
mkProd (Value x) (Value y) = Value (x*y)
mkProd mlpr mlpc = Product mlpr mlpc


instance (Show a) => Show (Equation a) where
    show (Value a) = show a
    show (Variable v) = show v
    show (Sum aug adnd) = show aug ++ " + " ++ show adnd
    show (Product m0 m1) = show m0 ++ "*" ++ show m1

instance (Num a, Eq a) => Num (Equation a) where
    (+) = mkSum
    (*) = mkProd
    signum (Value x) =  Value (signum x)
    signum v@(Variable _) = v
    signum (Sum x y) = Sum (signum x) (signum y)
    signum (Product x y) = Product (signum x) (signum y)
    abs (Value x) = Value (abs x)
    abs v@(Variable _) = v
    abs (Sum x y) = Sum (abs x) (abs y)
    abs (Product x y) = Product (abs x) (abs y)
    fromInteger = Value . fromInteger

deriv :: (Num a, Eq a) => Equation a -> Var -> Equation a
deriv (Value _) _ = Value 0
deriv (Variable e) var = Value (
      if e == var then 1
                  else 0
                  )
deriv (Sum aug adnd) var = mkSum (deriv aug var) (deriv adnd var)
deriv (Product mlpr mlpc) var =
        mkSum  (mkProd (deriv mlpr var) mlpc)
               (mkProd mlpr (deriv mlpc var))
