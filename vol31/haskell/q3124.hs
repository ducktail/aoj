import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Text.Printf

main :: IO ()
main = solve <$> f <*> f >>= printf "%.10f\n"
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Double
solve [a, b] [p, q, r] = fromIntegral (b * p + q * a + b * r) / fromIntegral (q + r)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
