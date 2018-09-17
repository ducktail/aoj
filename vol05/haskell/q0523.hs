import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve n <$> replicateM n (getl toInt) >>= mapM_ print
    main

solve :: Int -> [Int] -> [Int]
solve n xs = f T 0 (sort xs) (sort ([1..2*n] \\ xs))
  where
    f tn fld ts hs
      | null ts = [length hs, 0]
      | null hs = [0, length ts]
      | tn == T && null t2s = f H 0 ts hs
      | tn == T = f H (head t2s) (t1s ++ (tail t2s)) hs
      | tn == H && null h2s = f T 0 ts hs
      | otherwise = f T (head h2s) ts (h1s ++ (tail h2s))
      where
        (t1s,t2s) = partition (<= fld) ts
        (h1s,h2s) = partition (<= fld) hs
        
toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine

data Player = T | H deriving (Show,Eq)

