import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char (digitToInt)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [h, w] <- f
  solve h w <$> V.fromList <$> concat <$> replicateM h g >>= print
  where
    f = map read <$> words <$> getLine
    g = map digitToInt <$> getLine

solve :: Int -> Int -> Vector Int -> Int
solve h w v = runST $ do
  dp <- V.thaw v
  forM_ [1 .. w-1] $ \x -> do
    d1 <- VM.read dp (toIdx (x-1, 0))
    d2 <- VM.read dp (toIdx (x, 0))
    VM.write dp (toIdx (x, 0)) (d1 + d2)
  forM_ [1 .. h-1] $ \y -> do
    d1 <- VM.read dp (toIdx (0, y-1))
    d2 <- VM.read dp (toIdx (0, y))
    VM.write dp (toIdx (0, y)) (d1 + d2)
  forM_ [1 .. w-1] $ \x -> do
    forM_ [1 .. h-1] $ \y -> do
      d1 <- VM.read dp (toIdx (x-1, y))
      d2 <- VM.read dp (toIdx (x, y-1))
      d3 <- VM.read dp (toIdx (x, y))
      VM.write dp (toIdx (x, y)) (d3 + min d1 d2)
  VM.read dp (toIdx (w-1, h-1))
  where
    toIdx (x, y) = x + y * w
