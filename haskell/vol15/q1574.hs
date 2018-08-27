import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f <*> f >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve [n, m] as = maximum $ (head as - 1) : (n - last as) : map (flip div 2) (zipWith (-) (tail as) as)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
