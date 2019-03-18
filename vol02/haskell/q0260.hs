import Control.Applicative
import Control.Monad
import Data.List
import Data.Char (isSpace)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> f <*> f >>= print
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve ps js = let js' = sort js
                  l = sum ps + sum js 
              in f l 1 l js'
  where
    f mx c l [] = mx
    f mx c l (x:xs) = f (max mx ((c + 1) * (l - x))) (c + 1) (l - x) xs

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')
