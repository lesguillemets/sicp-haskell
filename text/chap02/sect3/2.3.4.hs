module Haffman where
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List
import Data.Maybe
import Data.Function
-- $setup
-- >>> import Data.List

frequencies :: Ord a => [a] -> M.Map a Int
frequencies xs = runST $ do
    m <- newSTRef M.empty
    forM_ xs $ \c -> do
        val <- M.lookup c <$> readSTRef m
        case val of
            (Just _) -> modifySTRef' m (M.adjust succ c)
            Nothing -> modifySTRef' m (M.insert c 1)
    readSTRef m

-- |
-- prop> (M.toAscList . frequencies) xs == (map (liftM2 (,) head length) . group . sort) (xs :: [Char])

data HTree a = Leaf { _symbol :: a, _weight :: Int}
             | Tree {
                    _left :: HTree a, _right :: HTree a,
                    _symbols :: [a], _weight :: Int
             }

instance Show a => Show (HTree a) where
    show = unlines . showPart where
        showPart (Leaf s w) = ["L : \"" ++ show s ++ "\" w:" ++ show w]
        showPart (Tree l r s w) =
            ("T: \"" ++ show s ++ "\" w:" ++ show w):
                concatMap (map ('\t':)) [showPart l, showPart r]

symbols :: HTree a -> [a]
symbols (Leaf f _) = return f
symbols (Tree _ _ s _) = s

mkTree :: HTree a -> HTree a -> HTree a
mkTree t0 t1 = Tree t0 t1 sys wei where
    sys = symbols t0 ++ symbols t1
    wei = _weight t0 + _weight t1

--
type Bit = Int
decode :: HTree a -> [Bit] -> [a]
decode _ [] = []
decode tree bits =
    let
        decode1 [] _ = []
        decode1 (b:bs) cBranch =
            case chooseBranch b cBranch of
                (Leaf symb _) ->  symb:decode1 bs tree
                t@(Tree {}) ->  decode1 bs t
    in
        decode1 bits tree


chooseBranch :: Bit -> HTree a -> HTree a
chooseBranch 0 = _left
chooseBranch 1 = _right
chooseBranch _ = error "invalid bit"

--
adjoinLeafset :: HTree a -> [HTree a] -> [HTree a]
adjoinLeafset x [] = [x]
adjoinLeafset x xs =
    let w = _weight x
        (pre,post) = span ((< w) . _weight) xs
        in
            pre ++ x:post

mkLeafSet :: [(a,Int)] -> [HTree a]
mkLeafSet = map (uncurry Leaf) . sortBy (compare `on` snd)

joinHaffman :: [HTree a] -> HTree a
joinHaffman [x] = x
joinHaffman (t0:t1:ts) = joinHaffman $ adjoinLeafset (mkTree t0 t1) ts

mkHaffman :: Ord a => [a] -> HTree a
mkHaffman = joinHaffman .  mkLeafSet . M.toAscList . frequencies

-- Ex 2.68
encode :: Ord a => HTree a -> [a] -> [Bit]
encode t = concatMap (fromJust . encodeSymbol t)

encodeSymbol :: Ord a => HTree a -> a -> Maybe [Bit]
encodeSymbol (Leaf s _) e = if s == e
                                then Just []
                                else Nothing
-- TODO : make it clean
encodeSymbol (Tree (Leaf s _) r _ _ ) e = if s == e
                                         then Just [0]
                                         else liftA (1:) (encodeSymbol r e)
encodeSymbol (Tree l (Leaf s _) _ _ ) e = if s == e
                                         then Just [1]
                                         else liftA (0:) (encodeSymbol l e)
encodeSymbol (Tree l@Tree{} r@Tree{} _ _) e
        | e `elem` _symbols l = liftA (0:) (encodeSymbol l e)
        | e `elem` _symbols r = liftA (1:) (encodeSymbol r e)
        | otherwise = Nothing
