import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n readLn >>= print

solve :: [Int] -> Int
solve = flip div 2 . (+ 1) . abs . subtract 1 . (* 2) . length . filter (== 0)
