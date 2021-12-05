import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> readLn <*> f >>= mapM_ print
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> [Int] -> [Int]
solve n as = [foldl f 1 $ (tail >>= zip) as , n]
  where
    f ct (b, a)
      | b <= a = ct + 1
      | otherwise = ct

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
