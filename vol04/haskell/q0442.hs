import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve n
  | n <= 3 = 2
  | otherwise = let (q, r) = n `divMod` 3
                in q + (n + r + 2) `div` 3
