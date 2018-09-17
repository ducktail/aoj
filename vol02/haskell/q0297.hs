import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  solve <$> replicateM n (getl $ wrds toInt) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map f
  where
    f [k, p] = let m = k `mod` p in if m == 0 then p else m

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
