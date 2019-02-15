module Main where
import Control.Monad (when)
import Expr
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  a <- getLine
  when (a /= ":q") $ do
    print $ uncurry eval $ run a
    main
