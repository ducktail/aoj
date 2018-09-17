import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.List (unfoldr, foldl')
import Data.List (sort)
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  solve n <$> replicateM n (sort <$> readil B.readInt <$> B.getLine) >>= print

solve :: Int -> [[Int]] -> Int
solve n = g . foldl' f S.empty
  where f st x = S.insert x st
        g st = n - S.size st

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
