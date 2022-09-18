import Control.Applicative

main :: IO ()
main = solve <$> readLn <*> readLn >>= print

solve :: Int -> Int -> Int
solve = flip (-)
