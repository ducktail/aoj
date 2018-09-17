import Control.Applicative
import Control.Monad
import Data.List
import Data.Bool (bool)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    ts <- replicateM n f
    m <- readLn
    solve ts <$> replicateM m g >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine
    g = (\[d, l] -> (head d, read l)) <$> words <$> getLine

solve :: [[Int]] -> [(Char, Int)] -> String
solve ts = bool "No" "Yes" . null . snd . foldl' f ((10, 10), ts)
  where
    f ((cx, cy), ts) ('N', l) = ((cx, cy + l), filter (\[x, y] -> x /= cx || y `notElem` [cy .. cy + l]) ts)
    f ((cx, cy), ts) ('S', l) = ((cx, cy - l), filter (\[x, y] -> x /= cx || y `notElem` [cy - l .. cy]) ts)
    f ((cx, cy), ts) ('E', l) = ((cx + l, cy), filter (\[x, y] -> y /= cy || x `notElem` [cx .. cx + l]) ts)
    f ((cx, cy), ts) ('W', l) = ((cx - l, cy), filter (\[x, y] -> y /= cy || x `notElem` [cx - l .. cx]) ts)
