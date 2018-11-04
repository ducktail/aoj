import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve n m <$> f >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [Int] -> Int
solve n m xs = h . head . groupBy g . sort . map f $ zip xs [1..]
  where
    f (x, i) = (m `div` x * x, x, i)
    g (t1, x1, _) (t2, x2, _) = t1 == t2 && x1 == x2
    h ys
      | length ys > 1 = n
      | otherwise = let (_, _, i) = head ys in i
