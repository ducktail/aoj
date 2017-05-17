import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  [a,b] <- getl $ wrds toInt
  unless (a == 0 && b == 0) $ do
    n <- getl toInt
    solve a b <$> replicateM n (getl $ wrds toInt) >>= print
    main

solve :: Int -> Int -> [[Int]] -> Int
solve a b xs = (! (a,b)) $ foldl g ia [(i,j) | i <- [1..a],j <- [1..b]]
  where
    f i j = [i,j] `elem` xs
    ia = listArray ((1,1),(a,b)) (repeat 0) :: UArray (Int,Int) Int
    g ar (i,j)
      | f i j = ar // [((i,j),0)]
      | i == 1 && j == 1 = ar // [((i,j),1)]
      | i == 1 = ar // [((i,j), ar ! (i,j-1))]
      | j == 1 = ar // [((i,j), ar ! (i-1,j))]
      | otherwise = ar // [((i,j), ar ! (i-1,j) + ar ! (i,j-1))]
        
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
