import Control.Applicative
import Control.Monad
import Data.List
import Data.Function (on, fix)
import Data.Bool (bool)

main :: IO ()
main = do
  n <- readLn
  replicateM n (solve <$> f <*> g) >>= mapM_ print
  where
    f = map read <$> words <$> getLine
    g = map head <$> words <$> getLine

solve :: [Int] -> String -> Int
solve ns ss = bool 0 1 . all f . map (map snd) . groupBy ((==) `on` fst) . sort $ zip ss ns
  where
    f [] = True
    f xs@(x:_) = [x,x+1,x+2] `intersect` xs == [x,x+1,x+2] && f (xs \\ [x,x+1,x+2]) ||
                 [x,x,x] `isPrefixOf` xs && f (xs \\ [x,x,x])
