import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f >>= putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> String
solve [a, b]
  | a == 1 = f (b - 1)
  | otherwise = f (b - a + 1)
  where
    f x
      | odd x = "ODD"
      | otherwise = "EVEN"

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
