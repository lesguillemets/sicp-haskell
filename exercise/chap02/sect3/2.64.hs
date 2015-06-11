import BTSet

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
    --     putStrLn "_____"
    --     putStrLn $ "n = " ++ show n
    --     print $ thisEntry
    --     putStrLn $ "leftSize : " ++ show leftSize
    --     putStrLn $ "leftTree is : " ++ show leftTree
    --     print $ rightElms
    --     putStrLn $ "rightTree is  :" ++ show rightTree
    --     print $ remainingElms
    let next = (Tree thisEntry leftTree rightTree, remainingElms)
    print next
    return next

main = do
    a <- verboseTree ([1,3..11]::[Int]) 6
    print $ a
