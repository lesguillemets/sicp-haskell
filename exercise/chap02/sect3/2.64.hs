import BTSet

fromOrderedList :: Ord a => [a] -> Tree a
fromOrderedList es = fst $ partialTree es (length es)

partialTree :: [a] -> Int -> (Tree a, [a])
partialTree elms 0 = (EmptyTree, elms)
partialTree elms n =
        let leftSize = (n-1) `div` 2
            rightSize = n - (leftSize +1)
            (leftTree, thisEntry:rightElms) = partialTree elms leftSize
            (rightTree,remainingElms) = partialTree rightElms rightSize
            in
                (Tree thisEntry leftTree rightTree, remainingElms)
-- |
-- >>> fromOrderedList [1,3..11]
-- 5(1(.,3(.,.)),9(7(.,.),11(.,.)))

verboseTree :: [Int] -> Int -> IO (Tree Int, [Int])
verboseTree elms 0 = do
    putStrLn "reached bottom!"
    print elms
    return (EmptyTree, elms)
verboseTree elms n = do
    let leftSize = (n-1) `div` 2
        rightSize = n - (leftSize + 1)
    (leftTree, thisEntry:rightElms) <- verboseTree elms leftSize
    (rightTree,remainingElms) <- verboseTree rightElms rightSize
    putStrLn "_____"
    putStrLn $ "n = " ++ show n
    print $ thisEntry
    putStrLn $ "leftSize : " ++ show leftSize
    putStrLn $ "leftTree is : " ++ show leftTree
    print $ rightElms
    putStrLn $ "rightTree is  :" ++ show rightTree
    print $ remainingElms
    return (Tree thisEntry leftTree rightTree, remainingElms)

main = do
    a <- verboseTree ([1,3..11]::[Int]) 6
    print $ a
