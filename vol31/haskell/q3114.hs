import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve as = let (_, i, _) = foldl' f (1, 1, head as) as in i
  where
    f (i, mi, ma) a
      | ma > a = (i + 1, i, a)
      | otherwise = (i + 1, mi, ma)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
