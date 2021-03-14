import Control.Applicative
import Control.Monad
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Text.Printf

main :: IO ()
main = do
  [n, t, l, b] <- f
  unless (n == 0 && t == 0 && l == 0 && b == 0) $ do
    ls <- replicateM l readLn
    bs <- replicateM b readLn
    solve n t ls bs >>= printf "%.10f\n"
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [Int] -> [Int] -> IO Double
solve n t ls bs = do
  vl <- VM.replicate (n + 1) False
  vb <- VM.replicate (n + 1) False
  forM_ ls $ \i -> VM.write vl i True
  forM_ bs $ \i -> VM.write vb i True
  vbd <- VM.replicate (n + 1) 0.0 :: IO (IOVector Double)
  vwt <- VM.replicate (n + 1) 0.0 :: IO (IOVector Double)
  VM.write vbd 0 1.0
  f t vl vb vbd vwt
  where
    f ct vl vb vbd vwt = do
      if ct == 0 then VM.read vbd n
      else do
        vnbd <- VM.replicate (n + 1) 0.0 :: IO (IOVector Double)
        vnwt <- VM.replicate (n + 1) 0.0 :: IO (IOVector Double)
        cpp vbd vnbd n
        forM_ [0 .. n-1] $ \i -> do
          p <- VM.read vbd i
          when (p > 0.0) $ do
            forM_ [1 .. 6] $ \d -> do
              let ni = if i + d > n then 2 * n - i - d else i + d
              b <- VM.read vb ni
              if b then adp vnbd 0 (p / 6.0)
              else do
                l <- VM.read vl ni
                if l then adp vnwt ni (p / 6.0)
                else adp vnbd ni (p / 6.0)
        forM_ [1 .. n-1] $ \i -> do
          p <- VM.read vwt i
          when (p > 0.0) $ cpp vwt vnbd i
        f (ct - 1) vl vb vnbd vnwt

cpp :: IOVector Double -> IOVector Double -> Int -> IO ()
cpp vs vd i = do
  x <- VM.read vs i
  VM.write vd i x

adp :: IOVector Double -> Int -> Double -> IO ()
adp v i p = do
  -- VM.modify v (+ p) i
  x <- VM.read v i
  VM.write v i (x + p)
