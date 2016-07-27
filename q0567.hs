import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  solve <$> getl (wrds toInt) <*> getl toInt <*> replicateM n (getl toInt) >>= print

solve :: [Int] -> Int -> [Int] -> Int
solve [a,b] c ds = f a c (sortBy (flip compare) ds)
  where
    f :: Int -> Int -> [Int] -> Int
    f m c [] = truncate (fromIntegral c / fromIntegral m)
    f m c (x:xs)
      | m * x > b * c = f (m+b) (c+x) xs
      | otherwise = f m c xs

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
