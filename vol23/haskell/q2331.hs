import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  solve n <$> replicateM n f >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> Int
solve n xs = subtract 1 $ runST $ do
  v <- VM.replicate 100003 0 :: ST s (STVector s Int)
  forM_ xs $ \[a, b] -> do
    s <- VM.read v a
    VM.write v a (s+1)
    t <- VM.read v (b+1)
    VM.write v (b+1) (t-1)
  forM_ [1..100002] $ \i -> do
    s <- VM.read v (i-1)
    t <- VM.read v i
    VM.write v i (s + t)
  foldM (\c i -> do
            s <- VM.read v i
            if s >= i - 1
              then return $ max c i
              else return c
         ) 1 [1..100001]
