import Control.Applicative

main :: IO ()
main = solve <$> readLn <*> readLn >>= print

solve :: Int -> Int -> Int
solve a = (+ 1) . flip mod 12 . subtract 1 . (+ a)
