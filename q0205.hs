import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  x <- getl toInt
  unless (x == 0) $ do
    xs <- replicateM 4 (getl toInt)
    mapM_ print . solve $ x : xs
    main
  
solve :: [Int] -> [Int]
solve xs
  | length ys == 2 = map (f ys) xs
  | otherwise = [3,3,3,3,3]
  where
    ys = sort . nub $ xs
    f [1,2] n = n
    f [1,3] n = if n == 3 then 1 else 2
    f [2,3] n = n - 1
    
toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
