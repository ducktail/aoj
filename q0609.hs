import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  n <- getl toInt
  m <- getl toInt
  solve n <$> getl (wrds toInt) <*> replicateM m (getl $ wrds toInt) >>= mapM_ print

solve :: Int -> [Int] -> [[Int]] -> [Int]
solve n ms ns = map sum $ transpose $ map elems $ f ms ns
  where
    f :: [Int] -> [[Int]] -> [UArray Int Int]
    f [] [] = []
    f (x:xs) (ys:yss) = accum (+) (listArray (1,n) zs) [(x,length $ filter (==0) zs)] : f xs yss
      where zs = map (\c -> if x == c then 1 else 0) ys

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
