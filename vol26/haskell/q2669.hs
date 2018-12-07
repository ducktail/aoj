import Control.Applicative

main :: IO ()
main = solve <$> getLine >>= putStrLn

solve :: String -> String
solve ss = case az ss of
            [] -> "-1"
            s -> s

az :: String -> String
az ss
  | null ss = []
  | null xs = []
  | null ys = []
  | otherwise = 'A' : 'Z' : az (tail ys)
  where
    (_, xs) = break (== 'A') ss
    (_, ys) = break (== 'Z') (tail xs)
