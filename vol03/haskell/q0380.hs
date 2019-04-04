import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve = flip div 2 . subtract 30
