import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- getc $ map toDbl . splitOn ","
  print $ solve xs

solve :: [[Double]] -> Double
solve (p:ps) = sum $ zipWith (\p1 p2 -> area p p1 p2) ps (tail ps)

area [x1,y1] [x2,y2] [x3,y3] = sqrt $ z * (z - a) * (z - b) * (z - c)
  where
    a = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
    b = sqrt $ (x2 - x3) ^ 2 + (y2 - y3) ^ 2
    c = sqrt $ (x3 - x1) ^ 2 + (y3 - y1) ^ 2
    z = (a + b + c) / 2.0

toDbl :: String -> Double
toDbl s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
