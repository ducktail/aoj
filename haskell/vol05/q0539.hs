import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  d <- getl toInt
  unless (d == 0) $ do
    n <- getl toInt
    m <- getl toInt
    ss <- replicateM (n-1) (getl toInt)
    hs <- replicateM m (getl toInt)
    print $ solve d n (0: (sort ss)) hs
    main

solve :: Int -> Int -> [Int] -> [Int] -> Int
solve d n ss = sum . map f
  where
    sa = listArray (0,n-1) ss :: Array Int Int
    dst x y = let z = abs (x - y) in min z (d - z)
    f h
      | h > sa ! (n-1) = min (dst h 0) (dst h (sa ! (n-1)))
      | otherwise = minimum $ map (dst h) $ search 0 (n-1) h
    search l r x
      | l + 1 == r = [sa ! l,sa ! r]
      | sa ! m > x = search l m x
      | otherwise = search m r x
      where
        m = (l + r) `div` 2

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
