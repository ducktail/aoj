import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = solve <$> readLn <*> f >>= print
  where
    f = head <$> B.words <$> B.getLine

solve :: Int -> ByteString -> Int
solve n bs = if n == 1 then 0 else snd . snd . B.foldl f (c1, (0, if c0 == c1 then 0 else 1)) $ bs''
  where
    Just (c0, bs') = B.uncons bs
    Just (c1, bs'') = B.uncons bs'
    f (c', (x, y)) c = if c == c' then (c, (y, y)) else (c, (y, max (x + 1) y))
