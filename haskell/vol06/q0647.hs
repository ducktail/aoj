import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (sortBy)

main :: IO ()
main = do
  [n, k] <- map read <$> words <$> getLine
  solve k <$> replicateM n (readi B.readInt <$> B.getLine) >>= print

solve :: Int -> [Int] -> Int
solve k xs = last xs + 1 - head xs - rt
  where rt = sum . take (k-1) . sortBy (flip compare) . (tail >>= zipWith (\a b -> a - b -1)) $ xs

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
