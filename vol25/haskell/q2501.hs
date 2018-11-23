import Control.Applicative

main :: IO ()
main = do
  n <- readLn
  [a, b] <- f
  [c, d] <- f
  print $ solve n (a-1) (b-1) (c-1) (d-1)
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Int -> Int -> Int -> Int
solve n a b c d = minimum $ map f [1 .. n]
  where
    f w = let (xa, ya) = divMod a w
              (xb, yb) = divMod b w
              (xc, yc) = divMod c w
              (xd, yd) = divMod d w
          in abs (xa - xb) + abs (ya - yb) + abs (xc - xd) + abs (yc - yd)
