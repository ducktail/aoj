import Control.Applicative
import Control.Monad
import Data.List
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

main :: IO ()
main = do
  m <- readLn
  n <- readLn
  solve m <$> replicateM n f >>= mapM_ print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> [Int]
solve m xys = loop [] s ndb
  where
    ndb = foldl' f (V.replicate (m+1) (0, [])) xys :: Vector (Int, [Int])
    f v [x, y] = let (nx, lx) = v ! x
                     (ny, ly) = v ! y
                 in v // [(x, (nx, y:lx)), (y, (ny+1, ly))]
    s = V.ifoldl' g [] ndb
    g l i (x, _)
      | i > 0 && x == 0 = i:l
      | otherwise = l
    loop :: [Int] -> [Int] -> Vector (Int, [Int]) -> [Int]
    loop ls [] db = reverse ls
    loop ls (x:xs) db = let (_, l) = db ! x
                            (db', s') = foldl' (\(v, s) i ->
                                                let (p, q) = v ! i
                                                in (v // [(i, (p-1,q))], if p == 1 then i:s else s)) (db, xs) l
                        in loop (x:ls) s' db'
