compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlpha:: Char -> Bool
isUpperAlpha= (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = (f x y) : (zipWith' f xs ys)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = (f x) : (map f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
    | p x = x : (filter' p xs)
    | otherwise  = (filter' p xs)

-- Partition with a predicate is probably better
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

sumOfOddSquares :: (Integral a) => a
sumOfOddSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : (chain (n `div` 2))
    | odd n = n : (chain (n*3 + 1))

chainLengthBiggerThan15 :: Int
chainLengthBiggerThan15 = length (filter longerThan15 (map chain [1..100]))
    where longerThan15 xs = length xs > 15

-- Partially applying a parameter to a bunch of functions created from `map`.
-- Pretty cool!
multipliers :: Int -> [Int]
multipliers number = map (\f -> f number) (map (*) [0..100])

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' target = foldl (\acc x -> if (x == target) then True else acc) False