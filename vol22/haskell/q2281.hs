import Control.Applicative
import Control.Monad
import Data.List
import Data.Char (ord, chr)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> (V.fromList <$> getLine) <*> replicateM n f >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine

solve :: Vector Char -> [[Int]] -> String
solve v cs = V.toList $ foldl' swp v (reverse cs)
  where
    swp v [i, j] = let i' = i - 1
                       j' = j - 1
                       d = abs (i - j)
                   in v // [(i', enc d (v ! j')),(j', enc d (v ! i'))]
    enc d c = let a = ord 'a' in chr $ (ord c - a + d) `mod` 26 + a
