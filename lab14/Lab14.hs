import Test.QuickCheck
import System.Random
import Data.Typeable

----------- Side effects - debugging pure functions

-- f :: Float -> Float
-- f x = x + 1

-- g :: Float -> Float
-- g x = x * 2

-- f' :: Float -> (Float,String)
-- f' x = (x + 1,"f was called.")

-- g' :: Float -> (Float,String)
-- g' x = (x * 2, "g was called.")

    
-- bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
-- bind f = (\(xg,sg) -> (fst (f xg), sg ++ snd (f xg)))

-- unit :: Float -> (Float,String)
-- unit x = (x,"")

-- lift :: (Float -> Float) -> (Float -> (Float,String))
-- lift f = unit . f

-- testEquality :: Float -> Bool
-- testEquality x = ((bind (lift f)).(lift g)) x == (lift (f.g)) x

----------- A Container : Multivalued functions

-- data Complex a = a :+ a
--     deriving (Show, Eq)

-- f :: Complex Double -> Complex Double
-- f (x :+ y) = (x+1) :+ (y+1)

-- g :: Complex Double -> Complex Double
-- g (x :+ y) = (x * 2) :+ (y * 2)


-- bind :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
-- bind f = (\listG -> concat [f lg| lg <- listG])


-- unit :: Complex Double -> [Complex Double]
-- unit c = [c]

-- lift :: (Complex Double -> Complex Double) -> (Complex Double -> [Complex Double])
-- lift f = unit . f

-- testEquality :: Double -> Bool
-- testEquality x = ((bind (lift f)).(lift g)) (x :+ x) == (lift (f.g)) (x :+ x)

------------- 
randoms' :: (RandomGen g, Random a) => g -> ([a],g)  
randoms' gen =
    let (value,newGen) = random gen
        (restOfList, finalGen) = randoms' newGen
    in  (value:restOfList,finalGen)

-------------

newtype Debuggable a = D (a,String)

instance Functor Debuggable where
    fmap f (D (a,s)) = D (f a,s)

instance Applicative Debuggable where
    pure a = D (a,"")
    D (f,s) <*> D (x,s') = D (f x,s ++ s')

instance Monad Debuggable where
    return x = D (x,"")
    D (x,s) >>= f = let D (x',s') = f x in D (x', s ++ s')

newtype Multivalued a = M {get :: [a]}

instance Functor Multivalued where
    fmap f (M list) = M (map f list)

instance Applicative Multivalued where
    pure a = M [a]
    M flist <*> M list = M [f x|  f <- flist, x <- list]

fs :: Multivalued (Int -> Int)
fs = M [\x -> x + 1, \x -> x - 1]

xs :: Multivalued (Int)
xs = M [1,2,3]

f :: Int -> Multivalued Int
f a = M [a,a+1,a+2]

joinTwo :: Multivalued a -> Multivalued a -> Multivalued a
joinTwo (M xs) (M ys) = M $ xs ++ ys

join :: [Multivalued a] -> Multivalued a
join [] = M []
join (x:xs) = x `joinTwo` (join xs)

instance Monad Multivalued where
    return a = M [a]
    M list >>= f = join [f x| x <- list]

newtype Randomised a = R {getDist :: StdGen -> (a,StdGen)}

instance Functor Randomised where
    fmap f (R dist) = R $ (\s -> let (value,s') = dist s in (f value,s'))

instance Applicative Randomised where
    pure a = R (\s -> (a,s))
    (R distF) <*> R dist = R (\s -> let (value,s') = dist s; (f,finalS) = distF s' in (f value,finalS) )

instance Monad Randomised where
    return = pure
    (R dist) >>= f =R (\s -> let (value,s') = dist s ; (R dist') = f value in dist' s')











