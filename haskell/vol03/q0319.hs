import Control.Applicative ((<$>), (<*>))
import Data.List (sort)

main :: IO ()
main = solve <$> readLn <*> (map read <$> words <$> getLine) >>= print

solve :: Int -> [Int] -> Int
solve n ps = maximum $ zipWith min [n, n-1 .. 1] (sort ps)
