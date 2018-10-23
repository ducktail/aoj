import Control.Applicative
import Control.Monad
import Data.List
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM n f >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> Int
solve n xs = let tm = V.fromList . concat $ map f xs
                 cm = V.replicate (n * n) (-1) :: Vector Int
             in fst $ foldl' (g tm []) (0, cm) [0 .. n*n-1]
  where
    f (x:y:rs) = n * y + x : f rs
    f [] = []
    g tm ac (c, m) i
      | m ! i == (-1) = g tm (i:ac) (c, m // [(i, c)]) (tm ! i)
      | m ! i == c = (c + 1, m)
      | otherwise = (c, m // (map (\j -> (j, m ! i)) ac))
