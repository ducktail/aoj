import Control.Applicative
import Data.Bool (bool)

main :: IO ()
main = solve <$> readLn <*> getLine >>= print

solve :: Int -> String -> Int
solve n s = foldl f 0 $ zip ioi s
  where
    f ct (x, y) = bool (ct + 1) ct $ x == y
    ioi = take n $ cycle "IO"
