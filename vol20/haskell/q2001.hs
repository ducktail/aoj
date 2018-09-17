import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, m, a] <- f
  unless (n == 0 && m == 0 && a == 0) $ do
    solve a <$> sortBy (flip compare) <$> replicateM m f >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> Int
solve = foldl' f
  where
    f a [_, p, q]
      | a == p = q
      | a == q = p
      | otherwise = a
