import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve n
  | 65 <= n && n <= 90 = 1
  | 97 <= n && n <= 122 = 2
  | otherwise = 0
