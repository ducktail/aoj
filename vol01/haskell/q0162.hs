import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, when, forM_)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  tbl <- VM.replicate 1000001 0 :: IO (IOVector Int)
  VM.write tbl 1 1
  forM_ [1 .. 500000] $ \i -> do
    x <- VM.read tbl i
    when (x > 0) $ do
      when (2 * i <= 1000000) $ VM.write tbl (2 * i) 1
      when (3 * i <= 1000000) $ VM.write tbl (3 * i) 1
      when (5 * i <= 1000000) $ VM.write tbl (5 * i) 1
  forM_ [1 .. 1000000] $ \i -> do
    x <- VM.read tbl i
    y <- VM.read tbl (i-1)
    VM.write tbl i (x + y)
  solve tbl

solve :: IOVector Int -> IO ()
solve tbl = do
  (m:rs) <- map read <$> words <$> getLine
  unless (m == 0) $ do
    let n = head rs
    (-) <$> VM.read tbl n <*> VM.read tbl (m-1) >>= print
    solve tbl
