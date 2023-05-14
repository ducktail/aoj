import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> f >>= print
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> Int
solve = f 0
  where
    f ct (x:y:z:zs)
      | x < y && y > z = f (ct + 1) (y:z:zs)
      | otherwise = f ct (y:z:zs)
    f ct _ = ct

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
