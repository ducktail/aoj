import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  solve <$> getl toInt <*> replicateM n (getl $ wrds toInt) >>= print

solve :: Int -> [[Int]] -> Int
solve m = fst . foldl' f (m,m)
  where
    f (s,t) [i,o]
      | t == (-1) = (0,-1)
      | t + i - o < 0 = (0,-1)
      | t + i - o > s = (t + i - o, t + i - o)
      | otherwise = (s, t + i - o)

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
