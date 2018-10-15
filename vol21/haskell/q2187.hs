import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    solve <$> f <*> f >>= (\p -> printf "%.7f %.7f\n" p (1.0 - p))
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> Double
solve xs ys = let gw = length $ filter (game xs) $ permutations ys
              in fromIntegral gw / tng
  where
    tng = fromIntegral . product $ [1 .. 9]

game :: [Int] -> [Int] -> Bool
game xs ys = (> 0) . sum $ zipWith f xs ys
  where
    f x y
      | x > y = x + y
      | otherwise = -(x + y)
