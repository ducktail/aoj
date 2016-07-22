import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array

main = do
  w <- inp toInt
  n <- inp toInt
  xs <- inps n (splt toInt)
  mapM_ print $ solve w xs
 
solve :: Int -> [[Int]] -> [Int]
solve w xs = elems narr
  where narr = foldl exchange iarr xs
        iarr = array (1,w) (zip [1..w] [1..w])
 
exchange :: Array Int Int -> [Int] -> Array Int Int
exchange arr [a, b] = arr // [(a,arr ! b),(b,arr ! a)]
 
toInt :: String -> Int
toInt s = read s
 
splt :: (String -> a) -> String -> [a]
splt f = map f . words . map (\s -> if s == ',' then ' ' else s)
 
inp :: (String -> a) -> IO a
inp f = f <$> getLine
 
inps :: Int -> (String -> a) -> IO [a]
inps n f = map f <$> replicateM n getLine
