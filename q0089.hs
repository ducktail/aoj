import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- getc $ map toInt . splitOn ","
  print $ solve xs

solve :: [[Int]] -> Int
solve xs = maxpath [] xs

maxpath :: [Int] -> [[Int]] -> Int
maxpath xs [] = head xs
maxpath xs (y:ys)
  | length xs > length y = maxpath (zipWith (+) (f xs) y) ys
  | otherwise = maxpath (zipWith (+) (g xs) y) ys
  where
    f zs = zipWith max zs (tail zs)
    g [] = [0]
    g zs = (head zs : f zs) ++ [last zs]
    
toInt :: String -> Int
toInt s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
