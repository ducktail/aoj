import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = getLine >> solve <$> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve as = fst $ IM.foldlWithKey f (0, 200) mp
  where
    mp = foldl (\m x -> IM.insertWith (+) x 1 m) IM.empty as
    f (x, c) k v
      | c > v = (k, v)
      | c == v = (min x k, c)
      | otherwise = (x, c)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
