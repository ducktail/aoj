import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.List (unfoldr)
import Data.List (foldl')
import Data.Bool (bool)

import Data.Char (isSpace)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> readil B.readInt <$> B.getLine >>= putStrLn
    main

solve :: Int -> [Int] -> String
solve n xs = bool "NO COLOR" (show k) $ v > (n `div` 2)
  where mp = foldl' (\m x -> IM.insertWith (+) x 1 m) IM.empty xs
        (k,v) = IM.foldlWithKey (\(k', v') k v -> if v' < v then (k, v) else (k', v')) (0,0) mp

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where g s = do
          (n, s') <- f s
          return (n, B.dropWhile isSpace s')
