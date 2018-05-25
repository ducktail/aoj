import Control.Applicative ((<$>))
import Control.Monad (replicateM_, forM_, unless, when)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  m <- readLn
  unless (m == 0) $ do
    tbl <- VM.replicate (mxi+1) 0 :: IO (IOVector Int)
    VM.write tbl 0 1
    replicateM_ m $ do
      [a, b] <- map read <$> words <$> getLine
      forM_ [mxi, mxi-1 .. 0] $ \i -> do
        x <- VM.read tbl i
        when (x > 0) $ do
          forM_ [a, 2*a .. a*b] $ \j -> do
            modify tbl (+ x) (i + j)
    g <- readLn
    replicateM_ g $ do
      n <- readLn
      VM.read tbl n >>= print
    main

mxi :: Int
mxi = 8000

modify :: IOVector Int -> (Int -> Int) -> Int -> IO ()
modify v f i = do
  x <- VM.read v i
  VM.write v i (f x)
