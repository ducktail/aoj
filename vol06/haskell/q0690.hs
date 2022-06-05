import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = B.getLine >> solve <$> f <*> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve as bs = length $ do
  a <- as
  b <- bs
  guard $ a <= b
  return ()

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
