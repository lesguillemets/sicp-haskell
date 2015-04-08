module TList where

data TList a = Nil | Cons (TList a) (TList a) | V a deriving (Show)
-- We should probably doing this with Cons [TList a] | V a
-- Note that his allows lists that does not terminate with Nil, like
-- Cons (V 2) (V 1)
list :: [TList a] -> TList a
list = foldr Cons Nil

fromList :: [a] -> TList a
fromList = list . map V

car :: TList a -> TList a
car (Cons p _) = p
car (V _) = error "car of V"
car Nil = error "car of Nil"
cdr :: TList a -> TList a
cdr (Cons _ q) = q
cdr (V _) = error "cdr of V"
cdr Nil = error "cdr of Nil"

unpack :: TList a -> a
unpack (V a) = a

countLeaves :: TList a -> Int
countLeaves Nil = 0
countLeaves (V _) = 1
countLeaves (Cons p q) = countLeaves p + countLeaves q
