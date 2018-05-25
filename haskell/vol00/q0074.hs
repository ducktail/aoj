import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Text.Printf

main :: IO ()
main = do
  [h,m,s] <- getl $ split toInt
  unless (h == (-1) && m == (-1) && s == (-1)) $ do
    mapM_ printTime $ solve (h,m,s)
    main

solve :: (Int,Int,Int) -> [(Int,Int,Int)]
solve x = [toTime t, toTime (3 * t)]
  where
    t = 7200 - fromTime x

printTime :: (Int,Int,Int) -> IO ()
printTime (h,m,s) = printf "%02d:%02d:%02d\n" h m s

fromTime :: (Int,Int,Int) -> Int
fromTime (h,m,s) = h * 3600 + m * 60 + s

toTime :: Int -> (Int,Int,Int)
toTime s = (s `div` 3600, s `mod` 3600 `div` 60, s `mod` 60)

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
