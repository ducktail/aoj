import Control.Applicative
import Data.Bool (bool)

main :: IO ()
main = solve <$> f >>= print
  where
    -- f = readil B.readInt <$> B.getLine
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve = bool 0 1 . (== 0) . flip mod 6 . sum
