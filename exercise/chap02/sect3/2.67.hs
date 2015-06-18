import Haffman

message :: [Int]
message = [0,1,1,0,0,1,0,1,0,1,1,1,0]

sampleTree :: HTree Char
sampleTree = mkTree (Leaf 'a' 4)
                    (mkTree (Leaf 'b' 2)
                            (mkTree (Leaf 'd' 1)
                                    (Leaf 'c' 1)
                            )
                    )

main = putStrLn $ decode sampleTree message
-- adabbca
