import Control.Applicative

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [x, y] = f ((0, 0), (0, 0)) clrs fibs dirs
  where
    f area (c:cs) (l:ls) (d:ds)
      | inArea (x, y) area = c
      | otherwise = f (expandArea d l area) cs ls ds

type Point = (Int, Int)
type Area = (Point, Point)
data Dir = E | N | W | S deriving Eq

fibs :: [Int]
fibs = 1 : 2 : (tail >>= zipWith (+)) fibs

clrs :: [Int]
clrs = cycle [1, 2, 3]
dirs = cycle [E, N, W, S]

inArea :: Point -> Area -> Bool
inArea (x, y) ((xl, yl), (xh, yh)) = xl <= x && x <= xh && yl <= y && y <= yh

expandArea :: Dir -> Int -> Area -> Area
expandArea dir len ((xl, yl), (xh, yh))
  | dir == E = ((xl, yl), (xh + len, yh))
  | dir == N = ((xl, yl), (xh, yh + len))
  | dir == W = ((xl - len, yl), (xh, yh))
  | otherwise = ((xl, yl - len), (xh, yh))
