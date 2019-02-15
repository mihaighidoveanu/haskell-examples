-- Informatics 1 - Functional Programming 
-- Declarative Programming 
-- Lab 5

import Data.Char
import Test.QuickCheck
import Data.List

-- 1. Map
-- a. (4 simboluri)
uppers :: String -> String
uppers s = map toUpper s

-- b. (7 simboluri)
doubles :: [Int] -> [Int]
doubles ar = map (\x -> x * 2) ar

-- c. (10 simboluri)
penceToPounds :: [Int] -> [Float]
penceToPounds ar = map (\p -> (fromIntegral p) / 100) ar

-- d. (11 simboluri)
uppers' :: String -> String
uppers' xs = [toUpper s | s <- xs]

-- (8 simboluri)
prop_uppers :: String -> Bool
prop_uppers xs = (uppers xs) == (uppers' xs)

-- 2. Filter
-- a. (4 simboluri)
alphas :: String -> String
alphas s = filter isAlpha s 

-- b. (8 simboluri)
rmChar ::  Char -> String -> String
rmChar c s = filter (\c' -> c' /= c) s

-- c. (8 simboluri)
above :: Int -> [Int] -> [Int]
above x ar = filter (\nr -> nr > x) ar 

-- d. (13 simboluri)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals ar = filter (\(x,y) -> x /= y) ar

-- e. (15 simboluri)
rmCharComp :: Char -> String -> String
rmCharComp c s = [c' | c' <- s, c' /= c] 

-- (11 simboluri)
prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s


-- 3. Comprehensions vs. map & filter
-- a. 
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- (7 simboluri)
upperChars' :: String -> String
upperChars' s = map toUpper (filter isAlpha s)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b. 
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- (13 simboluri)
largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (*2) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- (11 simboluri)
reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (\s -> even(length s)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs


-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

-- (7 simboluri)
productFold :: [Int] -> Int
productFold ar = foldr (*) 1 ar

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.  (16 simboluri)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x == True && andRec xs 

-- (7 simboluri)
andFold :: [Bool] -> Bool
andFold ar = foldr (&&) True ar

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.  (17 simboluri)
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x : xs) = x ++ concatRec xs

-- (8 simboluri)
concatFold :: [[a]] -> [a]
concatFold ar = foldr (++) [] ar

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.  (17 simboluri)
rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (x : xs) s = rmCharsRec xs (rmChar x s)

-- (6 simboluri)
rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]

-- 5
-- a. (10 simboluri)
uniform :: [Int] -> Bool
uniform ar =  all (\x -> x == (head ar) ) ar

-- b. (	 simboluri)
valid :: Matrix -> Bool
valid matrix = 
	uniform lengthsMatrix && length lengthsMatrix >= 2
 		where lengthsMatrix = map length matrix

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' f xs ys = map (uncurry f) (zip xs ys)

-- test
--prop_zipWith :: Eq c => (a -> b -> c) -> [a] -> [b] -> Bool
--prop_zipWith f xs ys = (zipWith' f xs ys) == (zipWith f xs ys)

-- 6.

-- 7.  (22 simboluri + 19 simboluri)  cu tot cu tratarea erorilor
plusM :: Matrix -> Matrix -> Matrix
plusM ma mb 
 | (not (valid ma)) || (not (valid mb)) = error "Parametrii functiei trebuie sa fie matrici valide"
 | otherwise = zipWith (zipWith (+)) ma mb

-- 8. (23 simboluri + 15 simboluri)  cu tot cu tratarea erorilor  
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum (zipWith (*) xs ys)

canMultiply :: Matrix -> Matrix -> Bool
canMultiply ma mb = length (transpose ma) == length mb


-- each row of timesM is a list of dotproducts between that row of ma and all the columns of mb
-- so we take each row of ma and turn it into a row of dotProducts with the transposed mb (this is first map)
-- the row of dotProducts is the transposed matrix mapped to dotProduct the row (this is the nested map)
timesM :: Matrix -> Matrix -> Matrix
timesM ma mb 
 | (not (valid ma)) || (not (valid mb)) = error "Parametrii functiei trebuie sa fie matrici valide"
 | not (canMultiply ma mb) = error "Matricile date nu se pot inmulti intre ele"
 | otherwise = map (\row -> map (dotProduct row) (transpose mb)) ma

