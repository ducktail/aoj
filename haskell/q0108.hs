import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  when (n /= 0) $ do
    xs <- getl $ split toInt
    solve 0 xs
    main

solve :: Int -> [Int] -> IO ()
solve n xs
  | oxs == xs = print n >> putStrLn (toStr xs)
  | otherwise = solve (n+1) oxs
  where
    oxs = opefreapp xs
    toStr = unwords . map show
    
opefreapp :: [Int] -> [Int]
opefreapp xs = foldr f [] xs
  where
    f n ls = let m = (length . filter (== n)) xs in m : ls

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
