import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,m] <- getl $ map read . words
  unless (n == 0 && m == 0) $ do
    solve <$> replicateM n (getl read) <*> replicateM m (getl read) >>= putStrLn . unwords . map show
    main

solve :: [Int] -> [Int] -> [Int]
solve xs ys
  | odd (sx + sy) = [-1]
  | otherwise = searchCards ((sy - sx) `div` 2) sxs sys
  where
    sxs = sort xs
    sys = sort ys
    sx = sum xs
    sy = sum ys

searchCards :: Int -> [Int] -> [Int] -> [Int]
searchCards _ [] _ = [(-1)]
searchCards _ _ [] = [(-1)]
searchCards d (x:xs) (y:ys)
  | x + d == y = [x, y]
  | x + d < y = searchCards d xs (y:ys)
  | otherwise = searchCards d (x:xs) ys

getl :: (String -> a) -> IO a
getl f = f <$> getLine
