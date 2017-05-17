import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main = do
  xs <- input (map read . splitOn ",") :: IO [[Int]]
  mapM_ putStrLn $ solve xs
 
solve :: [[Int]] -> [String]
solve = map poker
 
poker :: [Int] -> String
poker xs
  | cs == [1,4] = "four card"
  | sxs == [1,10,11,12,13] = "straight"
  | f sxs = "straight"
  | cs == [2,3] = "full house"
  | cs == [1,1,3] = "three card"
  | cs == [1,2,2] = "two pair"
  | cs == [1,1,1,2] = "one pair"
  | otherwise = "null"
  where
    cs = (sort . map length . group . sort) xs
    sxs = sort xs
    f [y] = True
    f (y:z:zs) = if y + 1 == z then f (z:zs) else False
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
