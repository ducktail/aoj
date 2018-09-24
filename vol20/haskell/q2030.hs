import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [a, b] <- f
  unless (a == 0 && b == 0) $ do
    print $ solve a b
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Int
solve a b = minimum $ do
  (x0, x1) <- divs a
  (y0, y1) <- divs b
  let [i, j, k, l] = sort [x0, x1, y0, y1]
  return $ (j - i) ^ 2 + (k - j) ^ 2 + (l - k) ^ 2

divs :: Int -> [(Int, Int)]
divs x = f 1 []
  where
    f d as
      | d ^ 2 > x = as
      | x `mod` d == 0 = f (d + 1) ((d, x `div` d) : as)
      | otherwise = f (d + 1) as
