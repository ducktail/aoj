import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- map ( map toDbl . words . rmc) . lines <$> getContents
  mapM_ print $ solve xs
 
solve :: [[Double]] -> [Int]
solve xs = [(truncate . getX) lp, (truncate. getY) lp]
  where lp = foldl move (point 0 0 90) xs
 
toDbl :: String -> Double
toDbl s = read s
 
rmc :: String -> String
rmc = map (\c -> if c == ',' then ' ' else c)

data Point = Point {getX::Double, getY::Double, getR::Double} deriving Show
 
point :: Double -> Double -> Double -> Point
point x y t = Point {getX = x, getY = y, getR = t * pi / 180.0}
 
move :: Point -> [Double] -> Point
move p [r, t] = Point {getX = getX p + r * cos (getR p), getY = getY p + r * sin (getR p), getR = getR p - t * pi / 180.0 }
