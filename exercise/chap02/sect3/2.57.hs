module TwoFiftySeven where
import Data.List
newtype Var = Var String deriving (Eq)
instance Show Var where
    show (Var s) = '\'' : s

data Equation a = Value a
                | Variable Var
                | Sum ([Equation a])
                | Product ([Equation a])

add :: (Eq a, Num a) => Equation a -> Equation a -> Equation a
add (Value 0) adnd = adnd
add aug (Value 0) = aug
add (Value x) (Value y) = Value (x+y)
add aug adnd = Sum [aug,adnd]

prod :: (Eq a, Num a) => Equation a -> Equation a -> Equation a
prod (Value 0) _ = Value 0
prod _ (Value 0) = Value 0
prod (Value 1) mlpc = mlpc
prod mlpr (Value 1) = mlpr
prod (Value x) (Value y) = Value (x*y)
prod mlpr mlpc = Product [mlpr,mlpc]

mkSum  :: (Eq a, Num a) => [Equation a] -> Equation a
mkSum = foldl1' add
mkProd  :: (Eq a, Num a) => [Equation a] -> Equation a
mkProd = foldl1' prod

instance (Show a) => Show (Equation a) where
    show (Value a) = show a
    show (Variable v) = show v
    show (Sum addens) = intercalate " + " (map show addens)
    show (Product ps) = '(' : intercalate ")*(" (map show ps) ++ ")"

instance (Num a, Eq a) => Num (Equation a) where
    (+) = add
    (*) = prod
    signum (Value x) =  Value (signum x)
    signum v@(Variable _) = v
    signum (Sum ss) = Sum (map signum ss)
    signum (Product ps) = Product (map signum ps)
    abs (Value x) = Value (abs x)
    abs v@(Variable _) = v
    abs (Sum ss) = Sum (map abs ss)
    abs (Product ps) = Product (map abs ps)
    fromInteger = Value . fromInteger

deriv :: (Num a, Eq a) => Equation a -> Var -> Equation a
deriv (Value _) _ = Value 0
deriv (Variable e) var = Value (
      if e == var then 1
                  else 0
                  )
deriv (Sum []) _ = Value 0
deriv (Sum (aug:adnd)) var = add (deriv aug var) (deriv (Sum adnd) var)
deriv (Product []) _ = Value 1
deriv (Product (mlpr:mlpc)) var =
        add  (prod (deriv mlpr var) (Product mlpc))
             (prod mlpr (deriv (Product mlpc) var))
