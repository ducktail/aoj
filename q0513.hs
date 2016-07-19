import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  m <- getl toInt
  solve n <$> replicateM m (getl toInt) >>= mapM_ print

solve :: Int -> [Int] -> [Int]
solve n = foldl shuffle [1..2*n]
  where
    shuffle ys i
      | i == 0 = concat $ zipWith (\a b -> [a,b]) (take n ys) (drop n ys)
      | otherwise = (drop i ys) ++ (take i ys)
                    
toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
