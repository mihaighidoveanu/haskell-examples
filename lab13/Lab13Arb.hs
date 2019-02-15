module Lab13Arb where

import Test.QuickCheck





--Exercise 3
data Arb a = Nil
            | Node a (Arb a) (Arb a)
             deriving (Show,Eq)
              
exTree = Node 4 (Node 25 Nil Nil) (Node 16 Nil Nil) :: Arb Float

instance Functor Arb where
    fmap f Nil = Nil
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)


instance Applicative Arb where
    pure x  =  Node x (pure x) (pure x)
    Nil <*> _   = Nil
    (Node f l r) <*> Nil = Nil
    (Node f l r) <*> (Node x l1 r1) = Node (f x) (l <*> l1) (r <*> r1)    
 
exComputation = (pure sqrt) <*> exTree

halve :: [a] -> ([a],[a])
halve xs 
 | ((length xs) == 0) = ([],[])
 | ((length xs) `mod` 2 == 0) = (take halfsize xs, drop halfsize xs)
 | otherwise = (take (halfsize + 1) xs, drop (halfsize + 1) xs)
 where halfsize = (length xs) `div` 2

listToArb :: [a] -> Arb a
listToArb [] = Nil
listToArb (x:xs) = 
    Node x (listToArb xs1) (listToArb xs2)
    where (xs1,xs2) = halve xs

testIdentity :: Eq a => Arb a -> Bool
testIdentity t =  (pure id <*> t) == t

testComposition = undefined

testHomomorphism :: Integer -> [Integer] -> Bool
testHomomorphism x ts = undefined

testInterchange = undefined











              
