import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve = flip mod 21
