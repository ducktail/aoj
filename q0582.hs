import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getc (map (wrds toInt)) >>= putStrLn . unwords . map show

solve :: [[Int]] -> [Int]
solve xs = foldr f [0,0,0,0] xs
  where
    f ys [n,t1,t2,t3]
      | c >= a + b = [0,0,0,0]
      | c ^ 2 == a ^ 2 + b ^ 2 = [n+1,t1+1,t2,t3]
      | c ^ 2 < a ^ 2 + b ^ 2 = [n+1,t1,t2+1,t3]
      | otherwise = [n+1,t1,t2,t3+1]
      where
        [a,b,c] = sort ys
        
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
