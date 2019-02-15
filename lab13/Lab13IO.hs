-- Lab 13
-- after https://www.schoolofhaskell.com/school/advanced-haskell/functors-applicative-functors-and-monads

import Lab13Parser as LP

-- Exercise 5


main :: IO()
main = 
    do
    putStrLn "Enter your birhtday year :"
    bys <- getLine
    let by = readYear bys
    if by == Nothing 
    then
        putStrLn "You entered invalid input"
    else
        do
        putStrLn "Enter a future year :"
        fys <- getLine
        let fy = readYear fys
        if fy == Nothing
        then 
            putStrLn "You entered invalid input"
        else
            do
            let d = maybeAge by fy 
            if positiveMaybe d
            then  
                putStrLn $ " In " ++ fys ++ " you will be " ++ show(maybe 0 id d) ++ " years old."
            else
                putStrLn "Birthday year cannot be greater than future year"
        



valid  :: [(Int,String)] -> Maybe Int
valid [] = Nothing
valid ((i,s) : xs) = Just i

readYear :: String -> Maybe Int
readYear str = valid (LP.parse LP.numberFourTimes str)

maybeAge :: Maybe Int -> Maybe Int -> Maybe Int
maybeAge by fy = (-) <$> fy <*> by

positiveMaybe :: Maybe Int -> Bool
positiveMaybe Nothing = False
positiveMaybe (Just i) = i >= 0

