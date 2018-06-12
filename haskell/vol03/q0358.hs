import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= putStrLn

solve :: [Int] -> String
solve [m, f, b] | lm < 0 = "0"
                | lm > f = "NA"
                | otherwise = show lm
  where lm = b - m
