import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = B.getLine >> solve <$> f <*> f >>= mapM_ print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> [Int]
solve = f
  where
    f [] bs = bs
    f as [] = as
    f (a:as) (b:bs)
      | a <= b = a : (f as (b:bs))
      | otherwise = b : (f (a:as) bs)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
