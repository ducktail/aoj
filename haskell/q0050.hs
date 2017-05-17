import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  ss <- getLine
  putStrLn $ solve ss
 
solve :: String -> String
solve s = f s
  where
    f [] = []
    f ('p':'e':'a':'c':'h':ss) = 'a':'p':'p':'l':'e': (f ss)
    f ('a':'p':'p':'l':'e':ss) = 'p':'e':'a':'c':'h': (f ss)
    f (x:xs) = x : (f xs)
