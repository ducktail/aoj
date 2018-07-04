import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = solve <$> readLn <*> readLn <*> readLn <*> readLn <*> readLn >>= print

solve :: Int -> Int -> Int -> Int -> Int -> Int
solve a b c d e | a < 0 = (-a) * c + d + b * e
                | otherwise = (b-a)*e
