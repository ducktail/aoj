import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- readLn :: IO Int
  xs <- input n (map read . words) :: IO [[Double]]
  mapM_ putStrLn $ solve xs
 
solve :: [[Double]] -> [String]
solve = map parallel
 
parallel :: [Double] -> String
parallel [x1, y1, x2, y2, x3, y3, x4, y4 ] | abs (crossp v1 v2) < eps = "YES"
                                           | otherwise = "NO"
  where v1 = Vec {getX = x2, getY = y2} `subVec` Vec {getX = x1, getY = y1}
        v2 = Vec {getX = x4, getY = y4} `subVec` Vec {getX = x3, getY = y3}
 
input :: Int -> (String -> a) -> IO [a]
input n f = map f <$> replicateM n getLine

data Vec = Vec {getX :: Double, getY :: Double}
 
crossp :: Vec -> Vec -> Double
crossp x y = getX x * getY y - getX y * getY x
 
subVec :: Vec -> Vec -> Vec
subVec v1 v2 = Vec {getX = getX v1 - getX v2, getY = getY v1 - getY v2}
 
eps :: Double
eps = 0.0000000001

