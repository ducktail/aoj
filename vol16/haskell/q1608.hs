import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- f
  unless (n == 0) $ do
    solve <$> g >>= print
    main
  where
    f = readi B.readInt <$> B.getLine
    g = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve = minimum . (tail >>= zipWith (-)) . sort

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
