import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isUpper)

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n B.getLine >>= B.putStrLn

solve :: [ByteString] -> ByteString
solve = B.concat . (++ [B.pack "UPC"]) . map (B.filter isUpper)
