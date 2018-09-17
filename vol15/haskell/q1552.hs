import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> tail <$> f >>= putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> String
solve = f (0, 0, 0, 0, 0) . map (\x -> (head x, length x)) . group
  where
    f (pl, bs, hs, pk, dp) xs@((h1,_):(h2,l2):(h3,_):_)
      | h2 > h1 && h2 > h3 && l2 > 1 = f (pl+1, bs, hs, pk, dp) (tail xs)
      | h2 < h1 && h2 < h3 && l2 > 1 = f (pl, bs+1, hs, pk, dp) (tail xs)
      | ((h1 < h2 && h2 < h3) || (h1 > h2 && h2 > h3)) && l2 > 1 = f (pl, bs, hs+1, pk, dp) (tail xs)
      | h2 > h1 && h2 > h3 = f (pl, bs, hs, pk+1, dp) (tail xs)
      | h2 < h1 && h2 < h3 = f (pl, bs, hs, pk, dp+1) (tail xs)
      | otherwise = f (pl, bs, hs, pk, dp) (tail xs)
    f (pl, bs, hs, pk, dp) _ = unwords . map show $ [pl, bs, hs, pk, dp]

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
