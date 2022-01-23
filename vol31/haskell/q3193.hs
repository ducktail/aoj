import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f >>= putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> String
solve [d, u, k, v]
  | bi < bu = "bike"
  | bi > bu = "bus"
  | otherwise = "same"
  where
    bi = d * v
    bu = u * (k * v + d)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
