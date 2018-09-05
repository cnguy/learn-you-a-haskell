maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot find the maximum of an empty list."
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : (replicate' (n - 1) x)

take' :: (Ord i, Num i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : (take' (n - 1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = (reverse' xs) ++ [x]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, baka!"
head' (x : _) = x

repeat' :: a -> [a]
repeat' x = x : (repeat' x)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : (zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' target (x : xs) = if (x == target) then True else elem' target xs