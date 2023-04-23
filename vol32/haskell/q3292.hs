import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> sort <$> f >>= print
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve (a:as) = maximum $ map length $ f [] [a] (a:as)
  where
    f as bs (x:y:ys)
      | x + 1 == y = f as (y:bs) (y:ys)
      | otherwise = f (bs:as) [y] (y:ys)
    f as bs _ = bs : as

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
