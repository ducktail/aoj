import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Text.Printf
import Data.Array.Unboxed

main :: IO ()
main = solve 1

solve :: Int -> IO ()
solve n = do
  x <- getl toInt
  unless (x == 0) $ do
    printf "Case %d:\n" n
    let ar = array ((0,0),(x-1,x-1)) $ zigzag x 0 0 1 :: UArray (Int,Int) Int
    mapM_ putStrLn $ fmt x $ elems ar
    solve (n+1)
    
zigzag :: Int -> Int -> Int -> Int -> [((Int,Int),Int)]
zigzag n i j k
  | i == n-1 && j == n-1 = [((i,j),k)]
  | even (i+j) && j == n-1 = ((i,j),k) : zigzag n (i+1) j (k+1)
  | even (i+j) && i == 0 = ((i,j),k) : zigzag n i (j+1) (k+1)
  | even (i+j) = ((i,j),k) : zigzag n (i-1) (j+1) (k+1)
  | odd (i+j) && i == n-1 = ((i,j),k) : zigzag n i (j+1) (k+1)
  | odd (i+j) && j == 0 = ((i,j),k) : zigzag n (i+1) j (k+1)
  | odd (i+j) = ((i,j),k) : zigzag n (i+1) (j-1) (k+1)

fmt :: Int -> [Int] -> [String]
fmt n xs
  | null xs = []
  | otherwise = concatMap (printf "%3d") (take n xs) : fmt n (drop n xs)

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
