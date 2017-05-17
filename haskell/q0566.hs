import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed
import Data.Function (on)

main :: IO ()
main = do
  n <- getl toInt
  solve n <$> replicateM (gamen n) (getl $ wrds toInt) >>= mapM_ print

solve :: Int -> [[Int]] -> [Int]
solve n = map fst . sortBy (compare `on` (fst . snd)) . g . zip [1..] . sortBy (flip compare `on` snd) . assocs . foldl f (listArray (1,n) (repeat 0))
  where
    f :: UArray Int Int -> [Int] -> UArray Int Int
    f ar [a,b,c,d]
      | c > d = accum (+) ar [(a,3)]
      | c < d = accum (+) ar [(b,3)]
      | otherwise = accum (+) ar [(a,1),(b,1)]
    g ((x1,(y1,z1)) : (x2,(y2,z2)) : rs)
      | z1 == z2 = (x1,(y1,z1)) : g ((x1,(y2,z2)) : rs)
      | otherwise = (x1,(y1,z1)) : g ((x2,(y2,z2)) : rs)
    g rs = rs
    
gamen :: Int -> Int
gamen n = n * (n-1) `div` 2

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
