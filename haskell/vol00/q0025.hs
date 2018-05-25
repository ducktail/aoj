import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- input $ map read . words :: IO [[Int]]
  mapM_ putStrLn $ solve xs
   
solve :: [[Int]] -> [String]
solve [] = []
solve (xs:ys:yss) = (show h ++ " " ++ show b) : solve yss
  where h = hit xs ys
        b = smn xs ys - h
 
hit :: [Int] -> [Int] -> Int
hit xs ys = sum $ zipWith (\x y -> if x == y then 1 else 0) xs ys
 
smn :: [Int] -> [Int] -> Int
smn xs ys = foldl f 0 ys
  where
    f :: Int -> Int -> Int
    f acc z = if z `elem` xs then acc + 1 else acc
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
