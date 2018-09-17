import Control.Applicative
import Control.Monad

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.List

import Data.Char (isSpace)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve n m <$> f >>= putStrLn
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> Int -> [Int] -> String
solve n m xs = maybe "NONE" show $ foldl' f Nothing $ do
  i <- [0 .. n - 2]
  j <- [i+1 .. n - 1]
  return (i, j)
  where
    v = V.fromList xs
    f mx (i, j)
      | v ! i + v ! j > m = mx
      | otherwise = case mx of
                     Nothing -> Just $ v ! i + v ! j
                     Just y -> Just $ max y (v ! i + v ! j)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
