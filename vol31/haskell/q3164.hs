import Control.Applicative
import Control.Monad (replicateM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n f >>= mapM_ putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: [[Int]] -> [String]
solve ats
  | null rs = ["No"]
  | otherwise = "Yes" : map (show . snd) rs
  where
    (h:as) = map f ats
    rs = filter (g h) $ zip as [2..]
    f [a, t]
      | t == 0 = a `div` 10 * 11
      | otherwise = a
    g h (x, _) = h > x

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
