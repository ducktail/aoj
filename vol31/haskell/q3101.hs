import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  B.getLine
  s <- B.getLine
  solve s

solve :: ByteString -> IO ()
solve s = do
  let (x, y) = B.foldl' f (0, 0) s
  print $ abs x + abs y
  B.putStr $ if y >= 0 then B.replicate y 'A' else B.replicate (-y) 'N'
  B.putStrLn $ if x >= 0 then B.replicate x 'a' else B.replicate (-x) 'n'
  where
    f (x, y) c
      | 'A' <= c && c <= 'M' = (x, y + 1)
      | 'N' <= c && c <= 'Z' = (x, y - 1)
      | 'a' <= c && c <= 'm' = (x + 1, y)
      | 'n' <= c && c <= 'z' = (x - 1, y)
      | otherwise = (x, y)
