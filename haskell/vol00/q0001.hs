import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  x:y:z:_ <- solve <$> replicateM 10 getLine
  print x
  print y
  print z
 
solve :: [String] -> [Int]
solve = reverse . sort . map read

-- main :: IO ()
-- main = do
--   xs <- inps 10 toInt
--   mapM_ print $ solve xs
-- 
-- solve :: [Int] -> [Int]
-- solve = take 3 . sortBy (\a b -> compare b a)
-- 
-- toInt :: String -> Int
-- toInt s = read s
-- 
-- inps :: Int -> (String -> a) -> IO [a]
-- inps n f = map f <$> replicateM n getLine

