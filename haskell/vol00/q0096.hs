import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = solve <$> map read <$> lines <$> getContents >>= mapM_ print

solve :: [Int] -> [Int]
solve = map (smv !)
  where smv = V.create $ do
          v1 <- VM.replicate 2001 0
          forM_ [0 .. 1000] $ \i -> do
            forM_ [0 .. 1000] $ \j -> do
              vij <- VM.read v1 (i+j)
              VM.write v1 (i+j) (vij+1)
          v2 <- VM.replicate 4001 0
          forM_ [0 .. 2000] $ \i -> do
            forM_ [0 .. 2000] $ \j -> do
              vi <- VM.read v1 i
              vj <- VM.read v1 j
              vij <- VM.read v2 (i+j)
              VM.write v2 (i+j) (vij + vi*vj)
          return v2
