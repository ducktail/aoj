import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, m, p] <- f
  solve n p <$> replicateM m readLn >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [Int] -> Int
solve n p ds = (* 100) $ loop (2 * n) sds
  where
    sds = sort (p : ds)
    cost l r = let cl = if l <= p then p - l else n - l + p
                   cr = if r >= p then r - p else n - p + r
               in if cl < cr then 2 * cl + cr else 2 * cr + cl
    loop mc (r:l:ls) = loop (min mc (cost l r)) (l:ls)
    loop mc [r] = min mc (cost (head sds) r)
