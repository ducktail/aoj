import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- f
  unless (all (== 0) xs) $ do
    solve xs <$> f >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> Int
solve [n, a, b, c, x] ys =
  let rs = unfoldr f (0, x)
  in g ys rs
  where
    f (i, x) = Just ((i, x), (i + 1, (a * x + b) `mod` c))
    g [] ((i,_):_) = i - 1
    g (y:ys) ((i,r):rs)
      | i > 10000 = (-1)
      | y == r = g ys rs
      | otherwise = g (y:ys) rs
