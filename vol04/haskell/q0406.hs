import Control.Applicative
import Data.List

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve n = maybe 0 id $ find (<= n) $ map (2^) [20, 19 .. 1]
