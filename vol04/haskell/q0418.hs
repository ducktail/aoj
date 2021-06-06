import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f <*> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve [x1, y1, w1, h1] [x2, y2, w2, h2] = w1 * h1 + w2 * h2 - 2 * area
  where
    cx1 = max x1 x2
    cy1 = max y1 y2
    cx2 = min (x1 + w1) (x2 + w2)
    cy2 = min (y1 + h1) (y2 + h2)
    area = max 0 (cx2 - cx1) * max 0 (cy2 - cy1)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
