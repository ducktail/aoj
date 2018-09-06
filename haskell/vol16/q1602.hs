import Control.Applicative
import Control.Monad
import Data.Char (digitToInt)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n getLine >>= print
    main

solve :: [String] -> Int
solve = snd . head . foldr f []
  where
    f s st
      | c == '+' = (l, sum . map snd $ as): bs
      | c == '*' = (l, product . map snd $ as): bs
      | otherwise = (l, digitToInt c):st
      where
        l = length s
        c = last s
        (as, bs) = span ((== l+1) . fst) st
