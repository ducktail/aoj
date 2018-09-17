import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,m] <- getl $ split toInt
  unless (n == 0 && m == 0) $ do
    print $ solve n m
    main
    
solve :: Int -> Int -> Int
solve n m = potato m (-1) n [1..n]

potato :: Int -> Int -> Int -> [Int] -> Int
potato m ix n xs
  | n == 1 = head xs
  | otherwise = potato m (dix - 1) (n - 1) ys
  where
    ys = delete (xs !! dix) xs
    dix = (ix + m) `mod` n
    
toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
