import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    (solve n) <$> replicateM n (getl $ wrds toInt) >>= mapM_ print
    main

solve :: Int -> [[Int]] -> [Int]
solve n xs = [ls !! 0, ls !! 1, ls !! (n - 2)]
  where
    toTime [c1, m1, s1, m2, s2, m3, s3, m4, s4] = (c1, (m1+m2+m3+m4)*60+s1+s2+s3+s4)
    ls = map fst . sortBy (compare `on` snd) . map toTime $ xs

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
