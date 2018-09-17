import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (find)

main :: IO ()
main = do
  [a, b] <- map read <$> words <$> getLine
  n <- readLn
  solve a b <$> replicateM n (map read <$> words <$> getLine) >>= print

solve :: Int -> Int -> [[Int]] -> Int
solve a b = maybe 0 (const 1) . find f
  where f [s, f] = min b f - max a s > 0
