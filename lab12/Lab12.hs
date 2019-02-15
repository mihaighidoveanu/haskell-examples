module Lab12 where

import Data.Char
import Data.List
import System.Environment


pb1 :: String -> String
pb1 xs = show (sum [read s :: Float | s <- words xs])

prog1 :: IO()
prog1 = 
    do
    line <- getLine
    if null line
        then
            return()
        else 
            do
            putStrLn $ pb1 line
            putStrLn ( unwords (reverse (words line)))
            putStrLn ( intercalate "," (sort $ words line) )
            putStrLn ""
            prog1

prog2 :: IO()
prog2 = 
    do
    line <- getLine
    prog2_aux (read line :: Int)

prog2_aux :: Int -> IO()
prog2_aux n = 
    do 
    if n == 0
        then 
            return()
        else
            do
            line <- getLine
            putStrLn $ show $ sum $ [read number :: Int| number <- words line]
            prog2_aux (n-1)

mapTuple :: (a->b) -> (a,a,a) -> (b,b,b)
mapTuple f (x,y,z) = (f x, f y, f z)

wc :: String -> (Int, Int, Int)
wc xs = (length $ lines xs,length $ words xs,length xs)

pairWithTab :: (String,String) -> String
pairWithTab (a,b) = a ++ ['\t'] ++ b

paste :: [String] -> [String] -> [String]
paste xs ys = 
    map pairWithTab pairs ++ drop (length pairs) xs ++ drop (length pairs) ys
    where pairs = zip xs ys

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

cut :: String -> Int -> [String] -> [String]
cut d column strings = [line!!(column - 1) | line <- map ( wordsWhen ( == (d!!0) )) strings ]

mainProgram :: IO()
mainProgram = 
    do
        params <- getArgs
        if null params
            then 
                print "Nu puteti rula programul fara argumente"
            else
                case params!!0 of
                    "wc" -> 
                        do
                        if length params == 2
                        then
                            do
                            file <- readFile $ params!!1
                            print $ wc file
                        else 
                            print $ "Se mai asteapta " ++ show (2 - length params ) ++ " argumente"
                    "paste" ->  
                        do
                        if length params == 3
                        then 
                            do
                            file1 <- readFile $ params!!1
                            file2 <- readFile $ params!!2
                            putStr $ unlines $ paste (lines file1) (lines file2)
                        else
                            print $ "Se mai asteapta " ++ show (2 - length params ) ++ " argumente"                            
                    "cut" -> 
                        do
                        if length params == 4
                        then
                            do
                            file <- readFile $ params!!3   
                            putStr $ unlines $ cut (params!!1) (read  ( params!!2) :: Int) (lines file)
                        else
                            print $ "Se mai asteapta " ++ show (2 - length params ) ++ " argumente"                            
                    other -> print "Optiune necunoscuta"



main :: IO ()
main = mainProgram
        

