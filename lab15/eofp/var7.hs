module Var7 where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
newtype M a = M { run :: ReaderT Environment (EitherT String (StateT Integer (WriterT String []))) a }

instance Monad M where
  return = pure
  (M ma) >>= k = M $ ma >>= \ a -> (run $ k a)

instance Applicative M where
  pure = M . return
  M mf <*> M ma = M $ (mf <*> ma)

instance Functor M where
  fmap f (M a) = M (f <$> a)

instance Alternative M where
  empty = M $ lift $ lift $ lift $ lift []
  m1 <|> m2 = M $
    ReaderT $ \ env ->
      EitherT $
        StateT $ \ i ->
          WriterT (runM m1 env i ++ runM m2 env i)


runM :: M a -> Environment -> Integer -> [((Either String a, Integer), String)]
runM ma env i = runWriterT $ runStateT (runEitherT $ runReaderT (run ma) env) i

formatValue :: Either String Value -> String
formatValue (Right x) = "Value: " ++ show x
formatValue (Left e)  = "Error: " ++ e

formatResult :: ((Either String Value, Integer), String) -> String
formatResult ((mv,cnt), w) = formatValue mv  ++ "; Count: " ++ show cnt ++ "; Output: " ++ w

showM :: M Value -> String
showM ma = concat ["Solution " ++ show i ++ ": " ++ formatResult x ++ "\n" | (i,x) <- solutions]
  where solutions = zip [1..] $ runM ma [] 0

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Count
          | Out Term
          | Fail
          | Amb Term Term
  deriving (Show)

term0 :: Term
term0 = App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)

term21 :: Term
term21 = App (Lam "x" (Var "x" :+: Var "y")) (Con 10 :+: Con 11)

term22 :: Term
term22 = App (Con 7) (Con 2)

term23 :: Term
term23 = Lam "x" (Var "x" :+: Var "x") :+: Con 11

-- for Variation three
term31 :: Term
term31 = (Con 1 :+: Con 2) :+: Count


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
interp (Con i) = return (Num i)
interp (t1 :+: t2) = do
  v1 <- interp t1
  v2 <- interp t2
  add v1 v2
interp (Lam x e) = do
  env <- M $ ask
  return $ Fun $ \ v -> M $ local (const ((x,v):env)) (run $ interp e)
interp (App t1 t2) = do
  f <- interp t1
  v <- interp t2
  apply f v
interp Count = do
  i <- fetchS
  return (Num i)
interp (Out t) = do
  v <- interp t
  M $ lift $ lift $ lift $ tell (show v ++ "; ")
  return v
interp Fail = empty
interp (Amb t1 t2) = interp t1 <|> interp t2

test1 :: WriterT String [] (Value, Integer)
test1 = WriterT [((Num 3, 0), "")]

test2 :: StateT Integer (WriterT String []) Value
test2 = StateT $ \ i -> WriterT [((Num 3, i), "")]

test3 :: EitherT String (StateT Integer (WriterT String [])) Value
test3 = EitherT $ StateT $ \ i -> WriterT [((Right $ Num 3, i), "")]
{-
  l1 <- interp t1
  l2 <- interp t2
  lift $ lift $ lift $ lift $ [l1, l2]

type M a =
  ReaderT Environment (
    EitherT String (
      StateT Integer (
        WriterT String []
      )
    )
  ) a
-}

tickS :: M ()
tickS = M $ lift $ lift $ modify (+ 1)

fetchS :: M Integer
fetchS = M $ lift $ lift get

throw :: String -> M a
throw =  M . lift . left

lookupM :: Name -> M Value
lookupM x = do
  env <- M $ ask
  do case lookup x env of
       Just v  -> return v
       Nothing -> throw ("unbound variable: " ++ x)

add :: Value -> Value -> M Value
add (Num i) (Num j) = tickS >> return (Num (i + j))
add v1 v2             = throw ("should be numbers: " ++ show v1 ++ ", " ++ show v2)

apply :: Value -> Value -> M Value
apply (Fun k) v = tickS >> k v
apply v1 _      = throw ("should be function: " ++ show v1)

test :: Term -> String
test t = showM $ interp t
