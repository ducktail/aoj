import Control.Applicative
import Control.Monad

main :: IO ()
main = solve <$> replicateM 3 f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [[Int]] -> Int
solve = g . sum . map f
  where
    f [x, y] = case (y - x) `mod` 3 of
      2 -> (-1)
      z -> z
    g x
      | x > 0 = 0
      | x < 0 = 1
      | otherwise = (-1)
