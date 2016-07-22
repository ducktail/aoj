import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- read <$> getLine :: IO Int
  xs <- map (map read . words) <$> replicateM n getLine :: IO [[Int]]
  mapM_ putStrLn $ solve xs
 
solve :: [[Int]] -> [String]
solve = map rightAngle
 
rightAngle :: [Int] -> String
rightAngle xs
  | x * x + y * y == z * z = "YES"
  | otherwise = "NO"
  where
    (x:y:z:_) = sort xs
