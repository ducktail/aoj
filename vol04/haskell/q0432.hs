import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  n <- readLn
  ss <- replicateM n B.getLine
  q <- readLn
  solve ss <$> (sum <$> replicateM q readLn) >>= mapM_ B.putStrLn

solve :: [ByteString] -> Int -> [ByteString]
solve ss r =
  case r `mod` 4 of
    1 -> rot180 . rot90 $ ss
    2 -> rot180 ss
    3 -> rot90 ss
    _ -> ss

rot180 :: [ByteString] -> [ByteString]
rot180 = reverse . map B.reverse

rot90 :: [ByteString] -> [ByteString]
rot90 = g []
  where
    f s (x, ys) = (B.cons (B.head s) x, B.tail s : ys)
    g xs ys
      | all B.null ys = xs
      | otherwise = let (a, bs) = foldr f (B.empty, []) ys in g (a : xs) bs
