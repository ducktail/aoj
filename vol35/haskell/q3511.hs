import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Data.Array.IO

main :: IO ()
main = do
  [h, w, n] <- f
  solve h w n >>= printArray
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> Int -> Int -> IO (IOUArray (Int, Int) Char)
solve h w n = do
  fld <- newArray ((1,1), (h, w)) '#' :: IO (IOUArray (Int, Int) Char)
  f fld n (1, 0) (cycle [(0, 1), (1, 0), (0, -1), (-1, 0)])
  where
    f :: IOUArray (Int, Int) Char -> Int -> (Int, Int) -> [(Int, Int)] -> IO (IOUArray (Int, Int) Char)
    f ar rct (i, j) dir = do
      if rct == 0 then return ar
      else do 
        let ((di, dj):dir') = dir
            ni = i + di
            nj = j + dj
        if ni < 1 || ni > h || nj < 1 || nj > w then f ar rct (i, j) dir'
        else do
          e <- readArray ar (ni, nj)
          if e == 'X' then f ar rct (i, j) dir'
          else do
            writeArray ar (ni, nj) 'X'
            f ar (rct - 1) (ni, nj) dir
    
printArray :: IOUArray (Int, Int) Char -> IO ()
printArray ar = do
  ((mnh, mnw),(mxh, mxw)) <- getBounds ar
  forM_ [mnh .. mxh] $ \i -> do
    forM_ [mnw .. mxw] $ \j -> do
      e <- readArray ar (i, j)
      putChar e
    putChar '\n'

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
