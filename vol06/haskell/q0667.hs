import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  [_, a, b] <- f
  solve a b <$> B.getLine >>= B.putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> Int -> ByteString -> ByteString
solve a b s = B.concat [s0, B.reverse s1, s2]
  where
    (s0, rs) = B.splitAt (a - 1) s
    (s1, s2) = B.splitAt (b - a + 1) rs

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
