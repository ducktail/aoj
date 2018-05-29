import Control.Applicative ((<$>))
import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n (map read <$> words <$> getLine) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map f
  where f [x, y, b, p] = min u v
          where u = x * b + y * p
                v = (x * (max 5 b) + y * (max 2 p)) * 4 `div` 5
