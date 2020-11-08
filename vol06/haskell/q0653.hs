import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  solve n <$> f <*> (getLine >> f) >>= mapM_ print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [Int] -> [Int] -> [Int]
solve n xs as = runST $ do
  vx <- V.thaw $ V.fromList xs
  forM_ as $ \i -> do
    p <- VM.read vx (i - 1)
    if i == n && p < 2019 then VM.write vx (i - 1) (p + 1)
      else when (i < n) $ do
        q <- VM.read vx i
        when (p + 1 /= q) $ VM.write vx (i - 1) (p + 1)
  V.toList <$> V.freeze vx
