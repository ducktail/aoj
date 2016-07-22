import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- getc $ split toInt
  mapM_ (putStrLn . unwords. map show) $ solve xs

solve :: [[Int]] -> [[Int]]
solve = map flyngjenny

flyngjenny :: [Int] -> [Int]
flyngjenny xs = head . sort . map snd $ takeWhile (\t -> fst t == (fst . head) zs) zs
  where
    zs = sort $ map f cs
    f ys = ((sum $ zipWith (\a b -> if a - b < 0 then 0 else a - b) xs ys), ys)

cs = [[1,4,1,4,1,2,1,2],[4,1,4,1,2,1,2,1],[1,4,1,2,1,2,1,4],[4,1,2,1,2,1,4,1],
     [1,2,1,2,1,4,1,4],[2,1,2,1,4,1,4,1],[1,2,1,4,1,4,1,2],[2,1,4,1,4,1,2,1]]

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
