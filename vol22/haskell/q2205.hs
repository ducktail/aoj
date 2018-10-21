import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve <$> replicateM n g <*> replicateM m getLine >>= print
    main
  where
    f = map read <$> words <$> getLine
    g = (\[a, b] -> (a, read b)) <$> words <$> getLine

solve :: [(String, Int)] -> [String] -> Int
solve xs ys = sum $ do
  (as, p) <- xs
  bs <- ys
  guard $ and $ zipWith f as bs
  return p
  where
    f a b = a == '*' || a == b
