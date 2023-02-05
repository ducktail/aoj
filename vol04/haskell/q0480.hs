import Control.Applicative

main :: IO ()
main = solve <$> f <*> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> Int
solve [ds, ms] [dh, mh]
  | ts >= 375 = 2
  | ts >= th = 1
  | otherwise = 0
  where
    ts = ds * 10 + ms
    th = dh * 10 + mh
