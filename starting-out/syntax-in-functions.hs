lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry buddy, you're out of luck!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1) 

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- My own function
-- flattenPairs [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
-- => [4,7,6,8,11,4]
flattenPairs :: (Num a) => [(a, a)] -> [a]
flattenPairs list = [a + b | (a, b) <- list]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, baka!"
head' (x : _) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty."
tell (x : []) = "The list has one element."
tell (x : y : []) = "The list has two elements."
tell (x : y : _) = "The list has more than two elements."

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (h : xs) = h + sum' xs

firstLetter :: String -> String
firstLetter "" = "Empty string, baka!"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight. Daijoubu."
    | bmi <= 25.0 = "You're normal. Daijoubu."
    | bmi <= 30.0 = "You're overweight. Daijoubu."
    | otherwise = "Nani... Baka!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight. Daijoubu."
    | bmi <= normal = "You're normal. Daijoubu."
    | bmi <= fat = "You're overweight. Daijoubu."
    | otherwise = "Nani... Baka!"
    where bmi = weight / (height^2)
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
    where (f : _) = firstName
          (l : _) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / (height^2)

describeList :: [a] -> String
describeList xs = "The list is " ++
    case xs of
        [] -> "empty."
        [x] -> "a singleton list."
        xs -> "a longer list."