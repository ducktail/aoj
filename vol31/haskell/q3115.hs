import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = B.getLine >> solve <$> f >>=  print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve = IS.size . IS.fromList

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
