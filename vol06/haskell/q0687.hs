import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = B.getLine >> solve <$> f >>= mapM_ print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int]
solve as = [sum us, sum vs - m]
  where
    m = maximum as
    (us, vs) = break (== m) as

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
