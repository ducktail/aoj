import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- getc $ split toInt
  mapM_ putStrLn $ solve xs

solve :: [[Int]] -> [String]
solve = map card

card :: [Int] -> String
card [c1,c2,c3] = if l >= 4 then "YES" else "NO"
  where
    cs = [1..10] \\ [c1, c2, c3]
    l = length $ filter (\x -> c1 + c2 + x <= 20) cs

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
