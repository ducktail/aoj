import Control.Monad (unless, forM_)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  tbl <- kannondou
  solve tbl

solve :: IOVector Int -> IO ()
solve tbl = do
  n <- readLn
  unless (n == 0) $ do
    VM.read tbl n >>= \x -> print $ (x + 3649) `div` 3650
    solve tbl

kannondou :: IO (IOVector Int)
kannondou = do
  tbl <- VM.replicate 33 0
  VM.write tbl 0 1
  forM_ [0 .. 29] $ \i -> do
    x <- VM.read tbl i
    modify tbl (+ x) (i+1)
    modify tbl (+ x) (i+2)
    modify tbl (+ x) (i+3)
  return tbl

modify :: IOVector Int -> (Int -> Int) -> Int -> IO ()
modify v f i = do
  x <- VM.read v i
  VM.write v i (f x)
