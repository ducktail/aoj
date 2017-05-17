import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- input (map read . words) :: IO [[Int]]
  mapM_ putStrLn $ solve xs
 
solve :: [[Int]] -> [String]
solve xs = map whatday $ takeWhile (\[a,_] -> a /= 0) xs
 
whatday :: [Int] -> String
whatday xs | w == 0 = "Wednesday"
           | w == 1 = "Thursday"
           | w == 2 = "Friday"
           | w == 3 = "Saturday"
           | w == 4 = "Sunday"
           | w == 5 = "Monday"
           | w == 6 = "Tuesday"
           | otherwise = "Rainyday"
  where w = yday xs `mod` 7
 
yday :: [Int] -> Int
yday [m,d] = sum (take m mds) + d
 
mds :: [Int]
mds = [0,31,29,31,30,31,30,31,31,30,31,30]
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
