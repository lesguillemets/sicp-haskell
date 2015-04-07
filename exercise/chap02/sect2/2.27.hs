module TwoTwentyseven where
import Prelude hiding (reverse)
import TList

reverse :: TList a -> TList a
reverse Nil = Nil
reverse (V a) = V a
reverse l@(Cons _ _) = let
    iter acc ss = case ss of
                      Nil -> acc
                      (V a) -> Cons (V a) acc
                      (Cons pre post) -> iter (Cons pre acc) post
                      in
                          iter Nil l

deepReverse :: TList a -> TList a
deepReverse Nil = Nil
deepReverse (V a) = V a
deepReverse (Cons x y) = case y of
                             Nil -> Cons x y
                             _ -> Cons (deepReverse y) (deepReverse x)
