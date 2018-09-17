import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl $ wrds toInt) >>= print
    main

solve :: [[Int]] -> Int
solve = sum . map toPrice
  where
    toPrice [x,y,h,w]
      | l <= 60 && w <= 2 = 600
      | l <= 80 && w <= 5 = 800
      | l <= 100 && w <= 10 = 1000
      | l <= 120 && w <= 15 = 1200
      | l <= 140 && w <= 20 = 1400
      | l <= 160 && w <= 25 = 1600
      | otherwise = 0
      where
        l = x + y + h

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
