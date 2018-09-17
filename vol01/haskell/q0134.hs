import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toIex
  solve n 0 0

solve :: Integer -> Integer -> Integer -> IO ()
solve n i sm
  | n == i = print (sm `div` n)
  | otherwise = do
      x <- getl toIex
      solve n (i+1) (sm + x)
      
toIex :: String -> Integer
toIex s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
