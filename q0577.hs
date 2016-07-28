import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  solve <$> replicateM n (getl $ wrds toInt) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map sum . transpose . map f . transpose
  where
    f xs = [if i `elem` (xs \\ [i]) then 0 else i | i <- xs]

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
