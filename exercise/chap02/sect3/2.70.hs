import Huffman

source :: [(String, Int)]
source = [
         ("a", 2),
         ("boom", 1),
         ("get", 2),
         ("job", 2),
         ("na", 16),
         ("sha", 3),
         ("yip", 9),
         ("wah", 1)
         ]
tree :: HTree String
tree = joinHuffman . mkLeafSet $ source

text :: [String]
text = concatMap words
        [
        "get a job",
        "sha na na na na na na na na",
        "get a job",
        "sha na na na na na na na na",
        "wah yip yip yip yip yip yip yip yip yip",
        "sha boom"
        ]

main = do
    let encoded = encode tree text
    mapM_ (putStr . show) encoded
    putStr "\n"
    print . length $ encoded
    print . (*3) . length $ text
