import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- inp toIex
  print $ fact n

fact :: Integer -> Integer
fact x
  | x == 1 = 1
  | otherwise = x * fact (x - 1)

toIex :: String -> Integer
toIex s = read s

inp :: (String -> a) -> IO a
inp f = f <$> getLine
