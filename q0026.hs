import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.Array

main = do
  xs <- input (map read . splitOn ",") :: IO [[Int]]
  mapM_ print $ solve xs
 
solve :: [[Int]] -> [Int]
solve xs = [length (filter (==0) rst), maximum rst]
  where rst = elems $ foldl inkdrop (listArray ((0,0),(9,9)) (repeat 0)) xs
         
     
inkdrop :: Array (Int, Int) Int -> [Int] -> Array (Int, Int) Int
inkdrop arr [x,y,s] = accum (+) arr $ map (\k -> (k,1)) rp
  where sp = [(x,y),(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        mp = sp ++ [(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]
        lp = mp ++ [(x+2,y),(x-2,y),(x,y+2),(x,y-2)]
        rp = if s == 1 then filter f sp
             else if s == 2 then filter f mp
                  else filter f lp
        f (i,j) = if i >= 0 && j >= 0 && i < 10 && j < 10 then True else False
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
