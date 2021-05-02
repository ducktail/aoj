import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve <$> replicateM m f >>= print
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: [[Int]] -> Int
solve = maximum . map sum . transpose

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
