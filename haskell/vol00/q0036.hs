import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- filter (/= "") . lines <$> getContents
  solve xs

solve :: [String] -> IO ()
solve [] = return ()
solve xs = do
  putStrLn $ figure fld
  solve $ drop 8 xs
  where
    fld = take 8 xs

figure :: [String] -> String
figure xs = s
  where
    rmr0 = filter (not . all (== '0')) xs
    ts = transpose rmr0
    rmc0 = filter (not . all (== '0')) ts
    Just s = lookup rmc0 tbl

tbl :: [([String],String)]
tbl = [(["11","11"],"A"),(["1111"],"B"),(["1","1","1","1"],"C"),(["011","110"],"D"),(["10","11","01"],"E"),(["110","011"],"F"),(["01","11","10"],"G")]
