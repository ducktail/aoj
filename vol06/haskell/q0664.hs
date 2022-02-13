import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = getLine >> solve <$> B.getLine >>= print

solve :: ByteString -> Int
solve = B.foldl' f 0
  where
    f x c = if c `elem` "aiueo" then x + 1 else x
