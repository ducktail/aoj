import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  k <- getl toInt
  solve n <$> replicateM k (getl $ wrds toInt) >>= mapM_ print

solve :: Int -> [[Int]] -> [Int]
solve n = map f
  where
    f [x,y] = g (x-1) (y-1)
    g x y
      | x + y > n - 1 = g (n - y - 1) (n - x - 1)
      | x - y < 0 = g y x
      | otherwise = y `mod` 3 + 1

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
