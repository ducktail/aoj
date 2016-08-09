import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  getc (solve . (map $ wrds toInt)) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map f
  where
    f [x,y] = gcd x y

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
