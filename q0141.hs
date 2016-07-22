import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  n <- getl toInt
  solve n 0

solve :: Int -> Int -> IO ()
solve n i = do
  unless (i == n) $ do
    unless (i == 0) $ putStrLn ""
    m <- getl toInt
    mapM_ putStrLn $ sp m
    solve n (i+1)

sp :: Int -> [String]
sp n = toPtn n $ elems (f iar n 1)
  where
    iar = listArray ((1,1),(n,n)) (repeat ' ') :: UArray (Int,Int) Char
    f :: UArray (Int,Int) Char -> Int -> Int -> UArray (Int,Int) Char
    f ar m i
      | m <= 4 = ar // (zip (g m i) (repeat '#'))
      | otherwise = f (ar // (zip (g m i) (repeat '#'))) (m-4) (i+2)
    g m i
      | m == 1 = [(i,i)]
      | m == 2 = [(i,i),(i+1,i),(i,i+1)]
      | m == 3 = [(i,i),(i+1,i),(i+2,i),(i,i+1),(i,i+2),(i+1,i+2),(i+2,i+2)]
      | m == 4 = [(i+j,i)| j <- [0..3]] ++ [(i,i+j)| j <- [1..3]] ++ [(i+j,i+3)| j <- [1..3]] ++ [(i+3,i+2)]
      | otherwise = [(i+j,i)| j <- [0..m-1]] ++ [(i,i+j)| j <- [1..m-1]] ++ [(i+j,i+m-1)|j <- [1..m-1]] ++ [(i+m-1,i+j)| j <- [2..m-2]] ++ [(i+m-2,i+2)]

toPtn :: Int -> String -> [String]
toPtn n [] = []
toPtn n xs = take n xs : toPtn n (drop n xs)

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

