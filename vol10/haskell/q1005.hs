import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM, unless)
import Data.List (transpose)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM n (map read <$> words <$> getLine) >>= print
    main

solve :: Int -> [[Int]] -> Int
solve n ts = case hs of
              (h : _) -> h
              _ -> 0
  where v1 = V.fromList (map minimum ts)
        v2 = V.fromList (map maximum (transpose ts))
        hs = [h1 | i <- [0..n-1], j <- [0..n-1], let h1 = v1 ! i, let h2 = v2 ! j, h1 == h2]
