import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array
import Data.List.Split (splitOn)

main :: IO ()
main = do
  n <- getl toInt
  when (n /= 0) $ do
    m <- getl toInt
    xs <- rgetl m $ map toInt . splitOn ","
    print $ solve n xs
    main
    
solve :: Int -> [[Int]] -> Int
solve n xs = mst adj 0 [0] [1..n-1]
  where
    adj = foldl (\ar [i,j,l] -> ar // [((i,j),l),((j,i),l)]) (listArray ((0,0),(n-1,n-1)) $ repeat infty) xs
    
mst :: Array (Int,Int) Int -> Int -> [Int] -> [Int] -> Int
mst adj s us vs
  | null vs = s
  | otherwise = mst adj (s + l `div` 100 - 1) (i:us) (delete i vs)
  where
    (l,(_,i)) = minimum [(adj ! (u,v),(u,v)) | u <- us, v <- vs]

infty :: Int
infty = 2000000000

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine
