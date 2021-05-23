import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Text.Printf

main :: IO ()
main = solve <$> f >>= printf "%.10f\n"
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Double
solve [a, t, r] = fromIntegral (t * r) / fromIntegral a

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
