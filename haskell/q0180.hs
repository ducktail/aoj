import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  [n,m] <- getl $ wrds toInt
  unless (n == 0 && m == 0) $ do
    solve n <$> replicateM m (getl $ wrds toInt) >>= print
    main
    
solve :: Int -> [[Int]] -> Int
solve n xs = mst am [0] [1..n-1] 0
  where
    ia = listArray ((0,0),(n-1,n-1)) $ repeat 10000 :: UArray (Int,Int) Int
    am = foldl (\ar [i,j,c] -> ar // [((i,j),c),((j,i),c)]) ia xs

mst :: UArray (Int,Int) Int -> [Int] -> [Int] -> Int -> Int
mst am us [] mnc = mnc
mst am us vs mnc = mst am (nu:us) (vs \\ [nu]) (mnc+tc)
  where
    (tc,nu) = minimum [(am ! (u,v), v)|u <- us, v <- vs]

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
