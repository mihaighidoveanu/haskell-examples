module Var6 where
import           Control.Monad.Reader
type M a = Reader Environment a

showM :: Show a => M a -> String
showM ma = show $ runReader ma []

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
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> M Value
interp (Var x) = lookupM x
interp (Con i) = return $ Num i
interp (Lam x e) = do
  env <- ask
  return $ Fun $ \ v -> 
    local (const ((x,v):env)) (interp e) 
interp (t1 :+: t2) = do
  v1 <- interp t1
  v2 <- interp t2
  add v1 v2
interp (App t1 t2) = do
  f <- interp t1
  v <- interp t2
  apply f v

lookupM :: Name -> M Value
lookupM x = do
  env <- ask
  case lookup x env of
    Just v  -> return v
    Nothing -> return Wrong

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add _ _             = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _       = return Wrong

test :: Term -> String
test t = showM $ interp t

pgmW :: Term
pgmW = App 
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))