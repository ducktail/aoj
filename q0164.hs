import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> getl (wrds toInt) >>= mapM_ print
    main

solve :: [Int] -> [Int]
solve xs = concat $ zipWith f [31,26,21,16,11,6,1] (cycle xs)
  where
    f x y = [x,max 0 (x-y)]

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
