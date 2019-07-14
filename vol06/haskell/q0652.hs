import Control.Applicative

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [a, b, c] = let cpw = 7 * a + b
                      (q, r) = c `divMod` cpw
                  in 7 * q + if r > 6 * a then 7 else (r + a - 1) `div` a
