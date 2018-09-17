import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 7 (getl $ wrds toInt) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map f
  where
    f [a, b] = a - b

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
