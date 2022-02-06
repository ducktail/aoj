import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Bool (bool)

import Data.Char (isSpace)

main :: IO ()
main = solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve = bool 1 2 . (>= 5) . sum

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
