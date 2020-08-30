import Control.Applicative
import Data.List

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [h, a, b] = length $ filter (\x -> a <= x && x <= b) ds
  where
    ds = concat $ unfoldr f $ takeWhile ((<= h) . (^ 2)) [1 ..]
    f [] = Nothing
    f (x:xs) = case h `divMod` x of
      (q, 0) | x == q -> Just ([x], xs)
             | otherwise -> Just ([x, q], xs)
      _ -> f xs
