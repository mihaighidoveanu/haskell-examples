data Dom a = Empty
            | Full 
            | Ran a a
            | (Dom a) 	:|: (Dom a)
            | (Dom a)  :&: (Dom a)
            deriving Show

exem :: Dom Int
exem = (((Ran 1 3) :|: (Ran 2 4)) :&: ((Ran 3 5) :&: Empty)) 


instance Eq a => Eq (Dom a) where
	(Ran x y) == (Ran u v) = ((x == u) && (y == v)) || ((x == v) && (y == u))
	Full == Full = True
	Empty == Empty = True
	left :|: right == left1 :|: right1 = (left == left1) && (right == right1)
	left :&: right == left1 :&: right1 = (left == left1) && (right == right1)
	_ == _ = False


exist :: Ord a => a -> Dom a -> Bool
exist v Empty = False
exist v Full = True
exist v (Ran x y) = (x <= v && v <= y) 
exist v (left :|: right) = (exist v left) || (exist v right)
exist v (left :&: right) = (exist v left) && (exist v right)

overlap :: Ord a => Dom a -> Dom a -> Bool
overlap (Ran a b) (Ran c d) =
  (a <= c && c <= b) ||  -- a < c < b
  (a <= d && d <= b) ||  -- a < d < b
  (c <= a && a <= d) ||  -- c < a < d
  (c <= b && b <= d)     -- c < b < d
overlap _ _ = False

normalize :: Ord a => Dom a -> Dom a
normalize Full = Full
normalize Empty = Empty
normalize r@(Ran _ _) = r
normalize ((x :|: y) :&: z) = normalize (x :&: z) :|: normalize (y :&: z)
normalize (x :|: y) = normalize x :|: normalize y
normalize (x :&: y) = normalize x :&: normalize y

newtype SDom a = S (Dom a)
instance Monoid (SDom a) where
	mempty = S (Empty)
	(S left) `mappend` (S right) = S (left :|: right)

newtype PDom a = P (Dom a)
instance Monoid (PDom a) where
	mempty = P (Full)
	(P left) `mappend` (P right) = P (left :&: right)


optimize :: Ord a => Dom a -> Dom a
optimize Empty = Empty
optimize r@(Ran _ _) = r

optimize (x :|: Empty) = optimize x  -- U
optimize (Empty :|: x) = optimize x  -- U
optimize (_ :&: Empty) = Empty
optimize (Empty :&: _) = Empty

optimize r@(Ran a b :|: Ran c d)  -- U
  | overlap (Ran a b) (Ran c d) = Ran (min a c) (max b d)
  | otherwise = r
optimize (Ran a b :&: Ran c d)
  | overlap (Ran a b) (Ran c d) = Ran (max a c) (min b d)
  | otherwise = Empty

optimize (x :|: y) = optimize $ optimize x :|: optimize y
optimize (x :&: y) = optimize $ optimize x :&: optimize y


