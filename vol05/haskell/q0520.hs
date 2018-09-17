import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, forM_)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM n (map read <$> words <$> getLine) >>= print
    main

solve :: Int -> [[Int]] -> Int
solve n xs = f v rt
  where v = V.create $ do
          v <- VM.replicate n (Node (-1) (-1) (-1) (-1) (-1))
          forM_ (zip [0 .. ] xs) $ \(i, [p, q, r, b]) -> do
            (Node _ _ pt _ _) <- VM.read v i
            let g = gcd p q
            VM.write v i (Node (p `div` g) (q `div` g) pt (r-1) (b-1))
            unless (r == 0) $ do
              (Node p q _ lc rc) <- VM.read v (r-1)
              VM.write v (r-1) (Node p q i lc rc)
            unless (b == 0) $ do
              (Node p q _ lc rc) <- VM.read v (b-1)
              VM.write v (b-1) (Node p q i lc rc)
          return v
        Just rt = V.findIndex (\nd -> pt nd == (-1)) v
        f v i | i == (-1) = 1
              | otherwise = l `div` p + l `div` q
          where (Node p q _ lc rc) = v ! i
                lw = f v lc
                rw = f v rc
                l = lcm (p * lw) (q * rw)

data Node = Node {p :: Int, q :: Int, pt :: Int, lc :: Int, rc :: Int } deriving Show
