import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (sort)

main :: IO ()
main = do
  [n, m] <- map read <$> words <$> getLine
  solve n <$> replicateM m (read <$> head <$> words <$> getLine) >>= print

solve :: Int -> [Int] -> Int
solve n = sum . map f . tail . sort
  where f x = max 0 (n - x)
