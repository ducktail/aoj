import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- getc $ splitOnSp toInt
  mapM_ putStrLn $ solve xs

solve :: [[Int]] -> [String]
solve xs = patrol [] $ splitOn [[0,0]] xs

patrol :: [String] -> [[[Int]]] -> [String]
patrol acc (x:xs)
  | null x = reverse acc
  | otherwise = patrol (judge x : acc) xs

judge :: [[Int]] -> String
judge xs = if ok then "OK" else "NG"
  where
    rmap = foldl (\im [a,b] -> IM.insertWith (+) b 1 (IM.insertWith (+) a 1 im)) IM.empty xs
    ok = odd (rmap IM.! 1) && odd (rmap IM.! 2) && ((== 2) . length . filter odd . IM.elems) rmap

toInt :: String -> Int
toInt s = read s

splitOnSp :: (String -> a) -> String -> [a]
splitOnSp f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
