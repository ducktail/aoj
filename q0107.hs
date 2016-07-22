import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [x,y] <- getl $ take 2 . sort . split toInt
  when (x /= 0 || y /= 0) $ do
    n <- getl toInt
    rs <- rgetl n toInt
    mapM_ putStrLn $ solve x y rs
    main
    
solve :: Int -> Int -> [Int] -> [String]
solve x y rs = map f rs
  where
    f r = if x ^ 2 + y ^ 2 < 4 * r ^ 2 then "OK" else "NA"

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine
