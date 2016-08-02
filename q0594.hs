import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [w,h,n] <- getl $ wrds toInt
  solve w h <$> replicateM n (getl $ toPoint . wrds toInt) >>= print

solve :: Int -> Int -> [Point] -> Int
solve w h xs = sum $ map f $ zip xs (tail xs)
  where
    f :: (Point,Point) -> Int
    f ((x0,y0), (x1,y1))
      | (x0 <= x1 && y0 >= y1) || (x0 >= x1 && y0 <= y1) = dx + dy
      | otherwise = let d = min dx dy in dx + dy - d
      where
        dx = abs (x0 - x1)
        dy = abs (y0 - y1)
        
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

type Point = (Int,Int)
toPoint :: [Int] -> Point
toPoint [x,y] = (x,y)
