data DomF a = Empty 				-- interval vid (multimea vida)
 			| Ran a a 				-- interval inchis [a,b]
 			| (DomF a) :|: (DomF a) -- reuniunea a 2 intervale A U B
 			deriving Show

instance Foldable DomF where
	foldr f i (left :|: right) = foldr f (foldr f i left) right
	foldr f i Empty = i
	foldr f i (Ran x y) = f y (f x i)

data Bin a = Leaf a
			| Node (Bin a) (Bin a)
			deriving (Show)

instance Foldable Bin where
	foldMap f (Leaf a) = f a
	foldMap f (Node l r) = (foldMap f l)  `mappend` (foldMap f r)




