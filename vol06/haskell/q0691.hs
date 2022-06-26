import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f <*> B.getLine >>= print
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> ByteString -> Int
solve [n, a] s = f 0 c $ mrg (a:reverse as ++ (cycle [0])) (bs ++ (cycle [n+1]))
  where
    (as, bs) = break (> a) $ B.findIndices (== '#') $ B.cons '.' s
    la = length as
    lb = length bs
    mrg (l:ls) (g:gs) = l : g : mrg ls gs
    c = if la < lb then lb * 2 - 1 else la * 2
    f tm ct (x:y:ys)
      | ct == 0 = tm
      | otherwise = f (tm + abs (x - y)) (ct - 1) (y:ys)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
