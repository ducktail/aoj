import Control.Applicative ((<$>))
main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve [h, r] | hr > 0 = 1
             | hr == 0 = 0
             | otherwise = (-1)
  where hr = h + r
