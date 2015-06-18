import Haffman

sampleTree :: HTree Char
sampleTree = mkTree (Leaf 'a' 4)
                    (mkTree (Leaf 'b' 2)
                            (mkTree (Leaf 'd' 1)
                                    (Leaf 'c' 1)
                            )
                    )
-- TODO : Add quickcheck

main = print $ encode sampleTree "adabbca"
