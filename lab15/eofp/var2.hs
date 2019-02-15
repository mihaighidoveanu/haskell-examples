module Var2 where
type M a = Either String a

showM :: Show a => M a -> String
showM (Left s)  = "Error: " ++ s
showM (Right a) = "Success: " ++ show a

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)


data Value = Num Integer
           | Fun (Value -> M Value)

type Environment = [(Name, Value)]

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) env = lookupM x env
interp (Con i) _ = return $ Num i
interp (t1 :+: t2) env = do
  v1 <- interp t1 env
  v2 <- interp t2 env
  add v1 v2
interp (Lam x e) env = return $ Fun $ \ v -> interp e ((x,v):env)
interp (App t1 t2) env = do
  f <- interp t1 env
  v <- interp t2 env
  apply f v

lookupM :: Name -> Environment -> M Value
lookupM x env = case lookup x env of
  Just v  -> return v
  Nothing -> Left ("unbound variable " ++ x)

add :: Value -> Value -> M Value
add (Num i) (Num j) = return $ Num $ i + j
add v1 v2           = Left
  ("should be numbers: " ++ show v1 ++ ", " ++ show v2)

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply v _       = Left ("should be function: " ++ show v)

test :: Term -> String
test t = showM $ interp t []

pgmE :: Term
pgmE = App
          (Var "x")
          ((Con 10) :+:  (Con 11))


