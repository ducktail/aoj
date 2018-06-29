import Control.Applicative ((<$>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = solve <$> B.getLine >>= print

solve :: ByteString -> Int
solve = joi 0

joi :: Int -> ByteString -> Int
joi lv bs | B.null bs = lv
          | otherwise = joi (max lv lv') rs
  where (lv', rs) = j bs

j :: ByteString -> (Int, ByteString)
j bs | B.null js = (0, B.empty)
     | otherwise = o (B.length js') rs
  where (_, js) = B.break (=='J') bs
        (js', rs) = B.span (=='J') js

o :: Int -> ByteString -> (Int, ByteString)
o lv bs | B.null os = (0, rs)
        | B.length os > lv = (0, rs)
        | otherwise = i (B.length os) rs
  where (os, rs) = B.span (=='O') bs

i :: Int -> ByteString -> (Int, ByteString)
i lv bs | B.null is = (0, rs)
        | B.length is < lv = (0, rs)
        | otherwise = (lv, rs)
  where (is, rs) = B.span (=='I') bs
