import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [h, w] <- f
  unless (h == 0 && w == 0) $ do
    solve h w <$> replicateM h getLine >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [String] -> Int
solve h w ss = runST $ do
  cv <- V.thaw $ V.fromList $ concat ss
  cnt <- VM.replicate ((h+1) * w) 0 :: ST s (STVector s Int)
  forM_ [0 .. h-1] $ \i -> do
    c <- VM.read cv (idx (i, 0))
    VM.write cnt (idx (i, 0)) (if c == '.' then 1 else 0)
    forM_ [1 .. w-1] $ \j -> do
      x <- VM.read cnt (idx (i, j-1))
      c <- VM.read cv (idx (i, j))
      VM.write cnt (idx (i, j)) (if c == '.' then x + 1 else 0)
  (mx, _) <- foldM (f cnt) (0, []) [(i, j) | j <- [0 .. w-1], i <- [0 .. h]]
  return mx
  where
    idx (i, j) = i * w + j
    f :: (STVector s Int) -> (Int, [(Int, Int)]) -> (Int, Int) -> ST s (Int, [(Int, Int)])
    f cnt (mx, st) (i, j) = do
      x <- VM.read cnt (idx (i, j))
      case st of
       [] -> return (mx, (x, i):st)
       ((rh, ri):rst) -> do
         if rh < x
           then return (mx, (x, i):st)
           else if rh > x
                then g (mx, st) (x, i, i)
                else return (mx, st)
    g :: (Int, [(Int, Int)]) -> (Int, Int, Int) -> ST s (Int, [(Int, Int)])
    g (mx, st) (x, i, pi) = do
      case st of
       [] -> return (mx, [(x, pi)])
       ((rh, ri):rst) -> do
         if rh < x
           then return (mx, (x, pi):st)
           else g ((max mx (rh * (i-ri))), rst) (x, i, ri)
