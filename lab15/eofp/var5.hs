module Var5 where
type M a = [a]

showM :: Show a => M a -> String
showM = show

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term
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
           | Wrong

type Environment = [(Name, Value)]

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"
  show Wrong   = "<wrong>"

interp :: Term -> Environment -> M Value
interp (Var x) env = get x env
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
interp Fail _ = []
interp (Amb t1 t2) env = interp t1 env ++ interp t2 env

get :: Name -> Environment -> M Value
get x env = case [v | (y,v) <- env , x == y] of
  (v:_) -> return v
  _     -> return Wrong

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add _ _             = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _       = return Wrong

test :: Term -> String
test t = showM $ interp t []
