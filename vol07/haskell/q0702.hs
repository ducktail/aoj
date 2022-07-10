import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace, isLower, toLower, toUpper)

main :: IO ()
main = do
  [_, k] <- f
  solve k <$> B.getLine >>= B.putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> ByteString -> ByteString
solve k s = B.append as $ B.map f bs
  where
    (as, bs) = B.splitAt (k - 1) s
    f c = if isLower c then toUpper c else toLower c

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
