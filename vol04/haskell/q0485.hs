import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [n, m] <- f
  solve n <$> replicateM m f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> [[Int]] -> Int
solve n ths = runST $ do
  v <- VM.replicate (n + 1) 0 :: ST s (STVector s Int)
  forM_ ths $ \[t, h] -> VM.write v h t
  (_, i) <- fs v 1
  loop v i i $ (sum . map head) ths
  where
    fs v i = do
      t <- VM.read v i
      if t > 0 then return (t, i)
        else fs v (i + 1)
    fd v i = do
      if i > n then return Nothing
        else do
        t <- VM.read v i
        if t == 0 then return $ Just i
          else fd v (i + 1)
    loop v ios iod tx = do
      (t, s) <- fs v ios
      r <- fd v iod
      case r of
        Just d -> do
          VM.write v s 0
          VM.write v d t
          loop v s d (tx + t)
        Nothing -> return tx

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
