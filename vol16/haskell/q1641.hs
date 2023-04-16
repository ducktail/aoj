import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [m, n, p] <- f
  unless (m == 0 && n == 0 && p == 0) $ do
    solve m p <$> replicateM n f >>= print
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> Int -> [[Int]] -> Int
solve m p abs = V.foldl f 0 $ runST $ do
  vif <- VM.replicate (m + 1) False
  VM.write vif p True
  forM_ abs $ \[a, b] -> do
    aif <- VM.read vif a
    when aif $ VM.write vif b True
    bif <- VM.read vif b
    when bif $ VM.write vif a True
  V.freeze vif
  where
    f ct b = if b then ct + 1 else ct

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
