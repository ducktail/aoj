import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve [p, m, c] = p + m + c
