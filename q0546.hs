import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  k <- getl toInt
  unless (n == 0 && k == 0) $ do
    solve k <$> replicateM n getLine >>= print
    main

solve :: Int -> [String] -> Int
solve k = length . nub . map concat . perm k
  where
    perm n xs
      | n == 1 = [[x] | x <- xs]
      | otherwise = [ys | x <- xs, ys <- map (x:) (perm (n-1) (xs \\ [x]))]

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
