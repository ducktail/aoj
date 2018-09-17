import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, forM)
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    [w, h] <- map read <$> words <$> getLine
    ps <- replicateM n ((\[x,y] -> (x, y)) <$> map read <$> words <$> getLine)
    [s, t] <- map read <$> words <$> getLine
    solve (w, h) ps (s, t) >>= print
    main

solve :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> IO Int
solve (w, h) ps (s, t) = do
  v <- VM.replicate ((w+1) * (h+1)) 0 :: IO (IOVector Int)
  forM [1..w] $ \i -> do
    forM [1..h] $ \j -> do
      p1 <- VM.read v (toIx (i-1, j-1))
      p2 <- VM.read v (toIx (i-1, j))
      p3 <- VM.read v (toIx (i, j-1))
      VM.write v (toIx (i, j)) (p2 + p3 - p1 + if (i, j) `elem` ps then 1 else 0)
  xs <- forM [1..w-s+1] $ \i -> do
    forM [1..h-t+1] $ \j -> do
      p1 <- VM.read v (toIx (i-1, j-1))
      p2 <- VM.read v (toIx (i-1, j+t-1))
      p3 <- VM.read v (toIx (i+s-1, j-1))
      p4 <- VM.read v (toIx (i+s-1, j+t-1))
      return $ p4 - p2 - p3 + p1
  return . maximum . concat $ xs
  where toIx (i, j) = (w+1) * j + i
