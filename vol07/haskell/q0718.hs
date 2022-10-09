import Control.Applicative
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [n, m] <- f
  solve n <$> replicateM m f >>= mapM_ print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> [Int]
solve n xys = V.toList $ V.create $ do
  v <- V.thaw $ V.fromList [1..n]
  forM_ xys $ \[x, y] -> VM.write v (x - 1) y
  return v
