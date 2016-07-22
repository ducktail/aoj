import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl $ wrds toInt) >>= mapM_ putStrLn
    main

solve :: [[Int]] -> [String]
solve = map f
  where
    f [m,e,j]
      | m == 100 || e == 100 || j == 100 = "A"
      | (m + e) >= 180 = "A"
      | (m + e + j) >= 240 = "A"
      | (m + e + j) >= 210 = "B"
      | (m + e + j) >= 150 && (m >= 80 || e >= 80) = "B"
      | otherwise = "C"

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
