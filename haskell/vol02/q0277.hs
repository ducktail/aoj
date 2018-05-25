import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 4 (getl $ wrds toInt) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map f
  where
    f [t,n] = g t * n
    g 1 = 6000
    g 2 = 4000
    g 3 = 3000
    g 4 = 2000

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
