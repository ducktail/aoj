import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [n, q] <- f
  as <- f
  mv <- V.thaw $ V.fromList $ sort as
  forM_ [1..q] $ \_ -> do
    [l, r] <- f
    solve mv n l r >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: IOVector Int -> Int -> Int -> Int  -> IO Int
solve v n l r = do
  li <- bsearch v n (>= l)
  ri <- bsearch v n (> r)
  return $ ri - li

bsearch :: IOVector Int -> Int -> (Int -> Bool) -> IO Int
bsearch v n f = g (-1) n
  where
    g :: Int -> Int -> IO Int
    g fi pi = do
      if pi == fi + 1 then return pi
      else do
        let i = (fi + pi) `div` 2
        x <- VM.read v i
        if f x then g fi i
        else g i pi

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
