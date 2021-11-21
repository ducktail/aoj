import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n B.getLine >>= print

solve :: [ByteString] -> Int
solve = length . filter (== e8)
  where
    e8 = B.pack "E869120"
