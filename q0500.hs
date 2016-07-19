import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    unwords . map show . solve <$> replicateM n (getl $ wrds toInt) >>= putStrLn
    main

solve :: [[Int]] -> [Int]
solve = foldl f [0,0]
  where
    f [sa,sb] [ta,tb]
      | ta > tb = [sa+ta+tb,sb]
      | ta < tb = [sa,sb+ta+tb]
      | otherwise = [sa+ta,sb+tb]

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
