import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  [n,m] <- getl $ wrds toInt
  solve n m <$> replicateM n (getl toInt) >>= mapM_ print

solve :: Int -> Int -> [Int] -> [Int]
solve n m xs = elems $ foldl f ia [1..m]
  where
    ia = listArray (1,n) xs :: UArray Int Int
    f a k = foldl g a [1..n-1]
      where
        g b i
          | b ! i `mod` k > b ! (i+1) `mod` k = b // [(i,b ! (i+1)),(i+1, b ! i)]
          | otherwise = b

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
