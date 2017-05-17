import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- input (map read . words) :: IO [[Double]]
  mapM_ putStrLn $ solve xs
 
solve :: [[Double]] -> [String]
solve = map f
  where
    f [xa1, ya1, xa2, ya2, xb1, yb1, xb2, yb2] = if frf (toRect [xa1, ya1, xa2, ya2]) (toRect [xb1, yb1, xb2, yb2]) then "NO" else "YES"
 
data Point = Point {xp :: Double, yp :: Double}
data Rect = Rect {ld :: Point, ru :: Point}
 
toRect :: [Double] -> Rect
toRect [x1, y1, x2, y2] = Rect (Point x1 y1) (Point x2 y2)
 
eps :: Double
eps = 0.0000000001
 
frf :: Rect -> Rect -> Bool
frf ra rb = s1 || s2 || s3 || s4
  where
    s1 = (xp . ld) rb - (xp . ru) ra > eps
    s2 = (xp . ld) ra - (xp . ru) rb > eps
    s3 = (yp . ld) rb - (yp . ru) ra > eps
    s4 = (yp . ld) ra - (yp . ru) rb > eps
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
