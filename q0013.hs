import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inpi toInt
  mapM_ print $ solve xs
 
solve :: [Int] -> [Int]
solve xs = reverse . snd $ foldl switch ([], []) xs
 
toInt :: String -> Int
toInt s = read s

inpi :: (String -> a) -> IO [a]
inpi f = map f . lines <$> getContents

switch :: ([Int], [Int]) -> Int -> ([Int], [Int])
switch (st, log) t
  | t == 0 = (tail st, head st : log)
  | otherwise = (t : st, log)

-- main = do
--   xs <- map toInt . words <$> getContents
--   mapM_ print $ switch [] [] xs
--  
-- toInt :: String -> Int
-- toInt s = read s
--  
-- switch :: [Int] -> [Int] -> [Int] -> [Int]
-- switch acc _ [] = reverse acc
-- switch acc (s:ss) (0:ts) = switch (s:acc) ss ts
-- switch acc ss (t:ts) = switch acc (t:ss) ts
