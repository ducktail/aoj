import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  nab <- f
  unless (sum nab == 0) $ do
    print $ solve nab
    main
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [n, a, b] = runST $ do
  vm <- VM.replicate (n + 1) False :: ST s (STVector s Bool)
  VM.write vm 0 True
  forM_ [a, b] $ \i -> do
    forM_ [0 .. n] $ \j -> do
      x <- VM.read vm j
      when (x && i + j <= n) $ VM.write vm (i + j) True
  v <- V.freeze vm
  return $ V.length $ V.filter not v
