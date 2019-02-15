module Var7 where
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
type M a = ReaderT Environment (EitherT String (State Integer)) a

type MRun a = (MEither a, Integer)
type MEither a = Either String a

runM :: M a -> Environment -> Integer -> MRun a
runM ma env i = runState (runEitherT $ runReaderT ma env) i

formatEither :: Show a => MEither a -> String
formatEither (Left s)  = "Error: " ++ s
formatEither (Right x) = "Success: " ++ show x

formatValue :: Show a => MRun a -> String
formatValue (ea, i) = formatEither ea ++ "; Count: " ++ show i

showM :: Show a => M a -> String
showM ma = formatValue $  runM ma [] 0

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

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"

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

throw :: String -> M a
throw = lift . left

tickS :: M ()
tickS = modify (+1)

lookupM :: Name -> M Value
lookupM x = do
  env <- ask
  case lookup x env of
    Just v  -> return v
    Nothing -> throw $ "variable not found: " ++ x

add :: Value -> Value -> M Value
add (Num i) (Num j) = tickS >> return (Num $ i + j)
add v1 v2           = throw $ "should be numbers: " ++ show v1 ++ ", " ++ show v2

apply :: Value -> Value -> M Value
apply (Fun k) v = tickS >> k v
apply v _       = throw $ "should be function: " ++ show v

test :: Term -> String
test t = showM $ interp t
