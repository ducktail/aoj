import Control.Applicative ((<$>))
import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n (map read <$> words <$> getLine) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map ((md -) . day)
  where md = day [1000, 1, 1]

day :: [Int] -> Int
day [y, m, d] = 195 * y' + y' `div` 3 * 5 + 20 * m' - (ly y') * (m' `div` 2) + d'
  where ly y = if y `mod` 3 == 2 then 0 else 1
        y' = y-1
        m' = m-1
        d' = d-1
