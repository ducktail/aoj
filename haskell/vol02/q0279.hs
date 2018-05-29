import Control.Applicative ((<$>))
import Control.Monad (unless)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.List (unfoldr, foldl')
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> readil B.readInt <$> B.getLine >>= putStrLn
    main

solve :: [Int] -> String
solve = g . foldl' f (0 , False)
  where f (ct, gt1) x = (ct + if x == 0 then 0 else 1, gt1 || x > 1)
        g (ct, gt1) | gt1 = show $ ct + 1
                    | otherwise = "NA"

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
