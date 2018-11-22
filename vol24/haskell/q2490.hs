import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n f >>= putStrLn
  where
    f = g <$> words <$> getLine
    g ["(", x] = read x
    g [")", x] = negate $ read x

solve :: [Int] -> String
solve = f 0
  where
    f b []
      | b == 0 = "YES"
      | otherwise = "NO"
    f b (x:xs)
      | b + x < 0 = "NO"
      | otherwise = f (b + x) xs
