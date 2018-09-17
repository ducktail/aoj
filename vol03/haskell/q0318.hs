import Control.Applicative ((<$>), (<*>))
import Data.List (intersect, (\\))

main :: IO ()
main = getLine >> solve <$> f <*> f <*> f >>= print
  where f = map read <$> tail <$> words <$> getLine

solve :: [Int] -> [Int] -> [Int] -> Int
solve xs ys zs = length (ys `intersect` zs) + length ((zs \\ ys) \\ xs)
