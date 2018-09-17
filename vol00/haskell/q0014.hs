import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- map toInt . words <$> getContents
  mapM_ print $ solve xs
 
solve :: [Int] -> [Int]
solve = map area
 
toInt :: String -> Int
toInt s = read s
 
area :: Int -> Int
area d = d * (sum $ map (\x -> x * x) ls)
  where ls = [d, d+d .. 600 - d]
