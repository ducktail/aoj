import Control.Applicative((<$>))
import Control.Monad (forM_, unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  ia <- getl (readiv B.readInt)
  ia' <- V.thaw ia
  insertionSort ia' n

insertionSort :: IOVector Int -> Int -> IO ()
insertionSort a n = do
  printa a n
  forM_ [1 .. n-1] $ \i -> do
    v <- VM.read a i
    let j = i - 1
    j' <- loop a j v
    VM.write a (j' + 1) v
    printa a n
  where
    loop :: IOVector Int -> Int -> Int -> IO Int
    loop a j v = do
      if j < 0 then return j
      else do
        u <- VM.read a j
        if u <= v then return j
        else do
          VM.write a (j+1) u
          loop a (j - 1) v

printa :: IOVector Int -> Int -> IO ()
printa a n = do
  forM_ [0 .. n-1] $ \i -> do
    unless (i == 0) $ putStr " "
    v <- VM.read a i
    putStr $ show v
  putStrLn ""

getl :: (ByteString -> a) -> IO a
getl f = f <$> B.getLine

readiv :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> Vector a
readiv f = V.unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
