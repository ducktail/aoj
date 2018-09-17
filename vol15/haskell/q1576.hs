import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [n, m] <- f
  solve n <$> replicateM m f >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> Int
solve n vss = abs . uncurry subtract . foldl' f (0, 0) $ [1..n]
  where
    f (c, v) i
      | tr ! i == (-1) = (c, v+1)
      | tr ! i < (-1) = (c+1, v)
      | otherwise = (c, v)
    tr = runST $ do
      tr <- VM.replicate (n+1) (-1) :: ST s (STVector s Int)
      forM_ vss $ \[x, y] -> do
        unite tr x y
      V.freeze tr

root :: STVector s Int -> Int -> ST s (Int, Int)
root tr i = do
  p <- VM.read tr i
  if p < 0
    then return (p, i)
    else do
      (c, p') <- root tr p
      VM.write tr i p'
      return (c, p')

unite :: STVector s Int -> Int -> Int -> ST s ()
unite tr i j = do
  (ci, pi) <- root tr i
  (cj, pj) <- root tr j
  unless (pi == pj) $ do
    if ci < cj
      then do
        VM.write tr pj pi
        VM.write tr pi (ci + cj)
      else do
        VM.write tr pi pj
        VM.write tr pj (ci + cj)
