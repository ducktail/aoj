import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, m] <- getl $ wrds toInt
  unless ( n == 0 && m == 0) $ do
    putStrLn $ solve n m
    main
    
solve :: Int -> Int -> String
solve n m = show r ++ " " ++ show c
  where
    (r, c) = eg (max n m) (min n m) 0

eg :: Int -> Int -> Int -> (Int, Int)
eg x y c
  | y == 0 = (x, c)
  | otherwise = eg y (x `mod` y) (c + 1)

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
