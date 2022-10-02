import Control.Applicative

main :: IO ()
main = solve <$> readLn <*> readLn <*> getLine >>= putStrLn

solve :: Int -> Int -> String -> String
solve n k s
  | r == k = "W"
  | otherwise = "R"
  where
    r = length $ filter (== 'R') $ s
