import Control.Applicative

main :: IO ()
main = solve <$> getLine >>= putStrLn

solve :: String -> String
solve s
  | head s == 'x' && last s == 'x' = "x"
  | otherwise = "o"
