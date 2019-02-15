-- Informatics 1 Functional Programming
-- Tutorial 8
--

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]
	

-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [length prod | (bar,(prod,units)) <- xs] 

formatLine :: Int -> (Barcode, Item) -> String
formatLine nr (bar, item) = bar ++ "..." ++ (fst item) ++ nr_puncte ++ "..." ++ (snd item)
 where nr_puncte = (replicate (nr - length(fst item)) '.')

showCatalogue :: Catalogue -> String
showCatalogue cs =foldr (\str str1 -> str ++"\n" ++ str1) ""  (map (formatLine (longestProductLen (toList cs)) ) (toList cs))
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe xs
 | length xs == 0 = Nothing
 | otherwise = (Just (head xs))

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [ a | m@(Just a) <- xs]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bs cat = catMaybes [get bar cat| bar <- bs] 






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
