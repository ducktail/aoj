import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getl toInt <*> getl (wrds toInt) >>= mapM_ print

solve :: Int -> [Int] -> [Int]
solve 2 [x,y] = cdiv $ x `gcd` y
solve 3 [x,y,z] = cdiv $ x `gcd` y `gcd` z

cdiv :: Int -> [Int]
cdiv x = sort $ f x 1
  where
    f y d
      | d ^ 2 > y = []
      | d ^ 2 == y = [d]
      | y `mod` d == 0 = d : (y `div` d) : f y (d+1)
      | otherwise = f y (d+1)

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
