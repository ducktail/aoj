import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  [n, m] <- f
  solve <$> f <*> f >>= mapM_ print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> [Int]
solve as bs = IS.toAscList $ IS.intersection sa sb
  where
    sa = IS.fromList as
    sb = IS.fromList bs

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
