import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.List (unfoldr)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    getLine
    solve n <$> B.getLine >>= print
    main

solve :: Int -> ByteString -> Int
solve n = sum . map f . unfoldr ioi
  where f p = max 0 (p - n + 1)

ioi :: ByteString -> Maybe (Int, ByteString)
ioi bs = f 0 (B.drop 1 bs1)
  where (_, bs1) = B.break (== 'I') bs
        f ct s | B.null s = Nothing
               | B.take 2 s == oi = f (ct+1) (B.drop 2 s)
               | otherwise = Just (ct, s)
        oi = B.pack "OI"
