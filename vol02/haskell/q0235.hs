import Control.Applicative
import Control.Monad
import Data.Tree
import Data.List
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM (n - 1) f >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> Int
solve n abts = let tr = unfoldTree g (0, 1)
               in 2 * lenTree tr - mxTree tr
  where
    v = foldl' f (V.replicate (n + 1) []) abts
    f v [a, b, t] = v // [(a, (b, t) : v ! a), (b, (a, t) : v ! b)]
    g (p, c)
      | p == 0 = ((c, 0), map (\(x, _) -> (c, x)) (v ! c))
      | otherwise = let ([(_, w)], us) = partition (\(x, _) -> x == p) (v ! c)
                    in ((c, w), map (\(x, _) -> (c, x)) us)
        
lenTree :: Tree (Int, Int) -> Int
lenTree (Node _ []) = 0
lenTree (Node (_, x) st) = x + sum (map lenTree st)

mxTree :: Tree (Int, Int) -> Int
mxTree (Node _ []) = 0
mxTree (Node (_, x) st) = x + maximum (map mxTree st)
