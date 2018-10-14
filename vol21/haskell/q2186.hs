import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    [gx, gy] <- f
    m <- readLn
    solve (gx, gy) <$> (S.fromList <$> replicateM m (g <$> f)) >>= putStrLn
  where
    -- f = readi B.readInt <$> B.getLine
    f = map read <$> words <$> getLine
    g [x1, y1, x2, y2] = if x1 > x2 || y1 > y2 then (x2, y2, x1, y1) else (x1, y1, x2, y2)

solve :: (Int, Int) -> Set (Int, Int, Int, Int) -> String
solve (gx, gy) st = let cnt = runST $ do
                          v <- VM.replicate ((gx + 1) * (gy + 1)) 0 :: ST s (STVector s Int)
                          forM_ [0 .. gy] $ \j -> do
                            forM_ [0 .. gx] $ \i -> do
                              ctd <- if S.notMember (i, j-1, i, j) st && j >= 1
                                        then VM.read v (toIdx (i, j-1))
                                        else return 0
                              ctl <- if S.notMember (i-1, j, i, j) st && i >= 1
                                        then VM.read v (toIdx (i-1, j))
                                        else return 0
                              VM.write v (toIdx (i, j)) (if i == 0 && j == 0 then 1 else ctd + ctl)
                          VM.read v (toIdx (gx, gy))
                    in if cnt > 0 then show cnt else "Miserable Hokusai!"
  where
    toIdx (i, j) = (gx + 1) * j + i
