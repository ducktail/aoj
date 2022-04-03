import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve = g . foldl f (0, 0, 0)
  where
    f (ml, cl, pv) x
      | pv <= x = (ml, cl + 1, x)
      | otherwise = (max ml cl, 1, x)
    g (x, y, _) = max x y
    
readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
