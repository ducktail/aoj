import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- (map read . lines) <$> getContents :: IO [Int]
  mapM_ print $ map (length . comb) xs
 
comb :: Int -> [(Int,Int,Int,Int)]
comb n = do
  a <- [0..9]
  b <- [0..9]
  c <- [0..9]
  d <- [0..9]
  guard $ a + b + c + d == n
  return (a,b,c,d)
