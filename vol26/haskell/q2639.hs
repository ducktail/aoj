import Control.Applicative

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [a, b, c] = f 0
  where
    f t
      | t == (a + b) = -1
      | otherwise = let d = 60 * t + c
                        u = d `div` (a + b)
                        mn = u * (a + b)
                        mx = mn + a
                    in if mn <= d && d <= mx
                       then d
                       else f (t + 1)
