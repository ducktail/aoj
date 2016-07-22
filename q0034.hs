import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main = do
  xs <- input (map read . splitOn ","):: IO [[Double]]
  mapM_ print $ solve xs
 
solve :: [[Double]] -> [Int]
solve = map railway
 
eps :: Double
eps = 0.0000000001
 
railway :: [Double] -> Int
railway xs = f 0 mls
  where
    ls = take 10 xs
    [v1,v2] = drop 10 xs
    t = sum ls / (v1 + v2)
    cp = v1 * t
    mls = scanl (+) 0 ls
    f n (y:ys) | cp - y > eps = f (n+1) ys
               | otherwise = n
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
