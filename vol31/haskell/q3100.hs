import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  [x, a, b] <- f
  n <- readLn
  solve x a b <$> replicateM n getLine >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> Int -> Int -> [String] -> Int
solve x a b = foldl f x
  where
    f l s
      | s == "nobiro" = max 0 (l + a)
      | s == "tidime" = max 0 (l + b)
      | otherwise = 0

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
