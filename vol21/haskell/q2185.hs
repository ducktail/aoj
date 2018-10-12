import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  m <- readLn
  replicateM_ m $ do
    fps <- f
    n <- readLn
    solve fps <$> replicateM n f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [[Int]] -> Int
solve [x, y, w, h] = length . filter f
  where
    f [cx, cy] = x <= cx && cx <= x + w && y <= cy && cy <= y + h
