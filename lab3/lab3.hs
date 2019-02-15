-- Declarative Programming
-- Lab 3
--

import           Data.Char
import           Data.List
import           Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = undefined

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec = undefined

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens = undefined



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = undefined

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec = undefined

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange = undefined



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives = undefined

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec = undefined

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives = undefined



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount = undefined

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher = undefined

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec = undefined

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher = undefined



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits = undefined

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec = undefined

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits = undefined



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise = undefined

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec = undefined

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise = undefined



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title = undefined

-- Recursive version
titleRec :: [String] -> [String]
titleRec = undefined

-- mutual test
prop_title :: [String] -> Bool
prop_title = undefined

