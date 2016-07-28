import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Arrow ((&&&))
import Control.Monad.State
import Data.List

main :: IO ()
main = do
  [d,n] <- getl $ wrds toInt
  solve <$> replicateM d (getl toInt) <*> replicateM n (getl $ wrds toInt) >>= print

solve :: [Int] -> [[Int]] -> Int
solve ds cs = h $ execState (mapM_ g (tail ls)) ((0,fst (head ls)),(0,snd (head ls)))
  where
    f x = (maximum &&& minimum) . map (!!2) . filter (\[a,b,_] -> a <= x && x <= b) $ cs
    ls = map f ds
    g :: (Int,Int) -> State ((Int,Int),(Int,Int)) ()
    g (h,l) = do
      ((th,xh),(tl,xl)) <- get
      let h1 = th + abs (h - xh)
          h2 = tl + abs (h - xl)
          l1 = th + abs (l - xh)
          l2 = tl + abs (l - xl)
      put ((max h1 h2, h),(max l1 l2,l))
    h ((x,_),(y,_)) = max x y
      
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
