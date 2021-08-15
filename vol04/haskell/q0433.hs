import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  [c1, c2] <- map head <$> words <$> getLine
  n <- readLn
  solve c1 c2 <$> replicateM n f >>= mapM_ putStrLn
  where
    f = readil B.readInt <$> B.getLine

solve :: Char -> Char -> [[Int]] -> [String]
solve c1 c2 xys = map (f tbl) xys
  where
    tbl = mktbl (color c1 c2)
    f t [x, y] = [t !! (x `mod` 4) !! (y `mod` 2)]

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace

color :: Char -> Char -> String
color c1 c2 = let Just s = find (\x -> [c1, c2] `isPrefixOf` x) tbl in s
  where
    tbl = ["BRYG", "BGRY", "BYGR",
           "RBGY", "RGYB", "RYBG",
           "GYRB", "GRBY", "GBYR",
           "YGBR", "YRGB", "YBRG"]
           
mktbl :: String -> [String]
mktbl [c1, c2, c3, c4] = [[c1, c3], [c2, c4], [c4, c2], [c3, c1]]
