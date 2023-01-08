import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve as = sum $ zipWith f (0:as) as
  where
    f x y = let d = abs (x - y) in min d (10 - d)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
