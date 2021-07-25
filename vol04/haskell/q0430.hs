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
solve (x : xs) =
  let (w, e) = minMax xs
  in if x < w then e - x
     else if x > e then x - w
          else if e - x > x - w then x - w + e - w
               else e - x + e - w
                  

minMax :: Ord a => [a] -> (a, a)
minMax (x:xs) = foldl f (x, x) xs
  where
    f (mn, mx) y = (min mn y, max mx y)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
