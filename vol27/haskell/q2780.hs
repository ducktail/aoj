import Control.Applicative
import Data.List

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve = maybe (-1) id . f Nothing
  where
    f mx [_] = mx
    f mx (x:xs) = let mx' = maximum $ (mx :) $ map Just $ filter inccons $ map (* x) xs
                  in f mx' xs

inccons :: Int -> Bool
inccons = and . (tail >>= zipWith f) . unfoldr g
  where
    f x y = x + 1 == y
    g 0 = Nothing
    g x = let (q, r) = divMod x 10
          in Just (r, q)
