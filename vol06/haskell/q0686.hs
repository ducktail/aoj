import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bool (bool)

main :: IO ()
main = B.getLine >> solve <$> B.getLine >>= B.putStrLn

solve :: ByteString -> ByteString
solve = bool (B.pack "No") (B.pack "Yes") . (== 0) . B.foldl f 3
  where
    f s c
      | s == 3 && c == 'I' = s - 1
      | s == 2 && c == 'O' = s - 1
      | s == 1 && c == 'I' = s - 1
      | otherwise = s
