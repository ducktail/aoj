import Control.Applicative

main :: IO ()
main = solve <$> getLine >>= print

solve :: String -> Int
solve s = let ls = length s
              (xs, ys) = splitAt (ls `div` 2) s
              y = head ys
          in sum (zipWith f xs (reverse ys))
             + if odd ls && (y == '(' || y == ')')
               then 1 else 0
  where
    f 'w' 'w' = 0
    f 'i' 'i' = 0
    f '(' ')' = 0
    f ')' '(' = 0
    f _ _ = 1
