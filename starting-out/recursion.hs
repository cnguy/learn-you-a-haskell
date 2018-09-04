maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot find the maximum of an empty list."
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : (replicate' (n - 1) x)