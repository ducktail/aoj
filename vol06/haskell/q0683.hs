import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = B.getLine >> solve <$> B.getLine >>= B.putStrLn

solve :: ByteString -> ByteString
solve = g . B.foldl f (0, 0, 0)
  where
    f (j, o, i) c
      | c == 'J' = (j + 1, o, i)
      | c == 'O' = (j, o + 1, i)
      | otherwise = (j, o, i + 1)
    g (j, o, i) = B.concat [B.replicate j 'J', B.replicate o 'O', B.replicate i 'I']
