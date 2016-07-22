import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  xs <- rgetl n $ split toInt
  putStrLn .  unwords . map show $ solve xs

solve :: [[Int]] -> [Int]
solve = head . sortBy (\a b -> if (a !! 1) == (b !! 1) then (a !! 0) `compare` (b !! 0) else (b !! 1) `compare` (a !! 1))

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine
