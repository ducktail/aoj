import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = getLine >> solve <$> f <*> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve as = f 0
  where
    f ln []  = ln
    f ln xs = f (max ln (search as xs)) (tail xs)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')

search :: [Int] -> [Int] -> Int
search xs ys = f 0 xs ys
  where
    f ln [] _ = ln
    f ln _ [] = ln
    f ln (x:xs) (y:ys)
      | x == y = f (ln + 1) xs ys
      | otherwise = f ln xs (y:ys)
