import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main = do
  xs <- input (map read . splitOn ",") :: IO [[Int]]
  mapM_ print $ solve xs
 
solve :: [[Int]] -> [Int]
solve xs = foldl f [0,0] xs
  where 
    f [r, l] [x,y,z] | x * x + y * y == z * z = [r+1,l]
                     | x == y = [r,l+1]
                     | otherwise = [r,l]
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
