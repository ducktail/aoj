import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [n, m, p] <- f
  unless (n == 0 && m == 0 && p == 0) $ do
    solve m p <$> replicateM n readLn >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [Int] -> Int
solve m p xs
  | nw == 0 = 0
  | otherwise = sum xs * (100 - p) `div` nw
  where
    nw = xs !! (m - 1)
