import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  [w, h] <- getl $ wrds toInt
  unless (w == 0 && h == 0) $ do
    solve w h <$> replicateM h getLine >>= print
    main

solve :: Int -> Int -> [String] -> Int
solve w h xs = length $ filter (\a -> snd a == '@') $ assocs $ search (iar // [(ipt,'.')]) ipt
  where
    iar = array ((0,0),(w-1,h-1)) [((i,j), xs !! j !! i) | i <- [0..w-1], j <- [0..h-1]] :: UArray (Int,Int) Char
    ipt = fst $ maybe ((-1,-1),'x') id $ find (\a -> snd a == '@') $ assocs iar
    search :: UArray (Int, Int) Char -> (Int, Int) -> UArray (Int, Int) Char
    search ar (i,j)
      | ar ! (i,j) == '.' = foldl search (ar // [((i,j),'@')]) ixs
      | otherwise = ar
      where
        ixs = [(ii,jj) | (ii,jj) <- [(i+1,j),(i-1,j),(i,j+1),(i,j-1)], ii >= 0, ii < w, jj >= 0, jj < h]

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
