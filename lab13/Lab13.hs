-- Lab 13

import Test.QuickCheck


-- Exercise 1

--fct1, fct2 :: ...
fct1 = Just (+3) 
fct2 = (+) <$> (Just 3)

 
fctTest :: Maybe Integer -> Bool
fctTest a = (fct1 <*> a) == (fct2 <*> a)


gen1, gen2 :: Integer -> Maybe (Integer -> Integer)
gen1 n = Just( + n )
gen2 n = (+) <$> (Just n)

 
genTest :: Integer -> Maybe Integer -> Bool
genTest a b  = (gen1 a <*> b) == (gen2 a <*> b)

-- Exercise 2

testIdentity :: Maybe Integer -> Bool
testIdentity v = (Just id <*> v) == v 

testComposition :: Integer -> Maybe Integer -> Bool
testComposition u w =( Just (.) <*> (Just (+u)) <*> (Just (*u)) <*> w ) ==(Just (+u) <*> (Just (*u) <*> w) )

testHomomorphism :: Integer -> Integer -> Bool
testHomomorphism x y = (Just (+x) <*> Just y) == (Just ((+x) y))

testInterchange :: Integer -> Integer -> Bool
testInterchange x y = (Just (*x) <*> Just y ) == ( (Just ($ y)) <*> Just (*x) )

              
