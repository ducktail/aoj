import Control.Applicative
import Data.Bool (bool)

main :: IO ()
main = solve <$> readLn <*> readLn <*> readLn  >>= print

solve :: Int -> Int -> Int -> Int
solve x y z = bool 0 1 $ x + y <= z
