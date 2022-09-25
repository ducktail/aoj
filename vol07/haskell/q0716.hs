import Control.Applicative

main :: IO ()
main = solve <$> readLn <*> readLn <*> readLn >>= print

solve :: Int -> Int -> Int -> Int
solve s a b = (max (s - a) 0 + b - 1) `div` b * 100 + 250
