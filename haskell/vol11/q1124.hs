import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (sort, group, maximumBy)

main :: IO ()
main = do
  [n,q] <- map read <$> words <$> getLine
  unless (n == 0 && q == 0) $ do
    solve q <$> concat <$> (replicateM n (tail <$> map read <$> words <$> getLine)) >>= print
    main

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve q xs = g . maximumBy f . group . sort $ xs
  where f xs ys | lx == ly = compare (head ys) (head xs)
                | otherwise = compare lx ly
          where lx = length xs
                ly = length ys
        g xs | length xs >= q = head xs
             | otherwise = 0
