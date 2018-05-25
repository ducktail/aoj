import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    print $ solve 0 n
    main

solve :: Int -> Int -> Int
solve c x
  | x == 1 = c
  | otherwise = solve (c + 1) (f x)
  where
    f n | even n = n `div` 2
        | otherwise = 3 * n + 1

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
