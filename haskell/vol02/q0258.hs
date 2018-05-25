import Control.Applicative ((<$>))
import Control.Monad (unless)

import Data.List (sort, sortBy, group)
import Data.Function (on)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> map read <$> words <$> getLine >>= print
    main

solve :: [Int] -> Int
solve xs = f xs
  where l5 = take 5 xs
        d = head $ head $ sortBy g $ group $ sort $ zipWith (-) (tail l5) l5
        g us vs | length us == length vs = compare (head vs) (head us)
                | otherwise = compare (length vs) (length us)
        f (x:y:z:zs) | y - x == d && z - y == d = f (y:z:zs)
                     | y - x == d = z
                     | z - y == d = x
                     | otherwise = y
