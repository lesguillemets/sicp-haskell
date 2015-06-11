module TwoSixtySix where
import BTSet

type Key = Int
type Datum a = (Key, a)

key :: Datum a -> Key
key = fst

value :: Datum a -> a
value = snd

lookUpSet :: Key -> Tree (Datum a) -> Maybe a
lookUpSet _ EmptyTree = Nothing
lookUpSet k (Tree e l r) =
        let entryKey = key e
            in
                case k `compare` entryKey of
                    EQ -> Just (value e)
                    GT -> lookUpSet k r
                    LT -> lookUpSet k l

-- $setup
-- >>> :{
--   let dataset = fromOrderedList
--          ([(1,'t'), (2,'5'), (23,'x'), (542,'2')] :: [(Int,Char)])
-- :}

-- |
-- >>> lookUpSet 2 dataset
-- Just '5'
--
-- >>> lookUpSet 3 dataset
-- Nothing
