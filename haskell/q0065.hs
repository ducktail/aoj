import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  (lm, tm) <- gettd . lines <$> getContents
  mapM_ (\(x, y) -> putStrLn $ unwords [show x, show y]) $ solve lm tm

solve :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
solve lm tm = map g isd
  where
    f = map (\x -> ((fst . head) x, length x)) . groupBy (\x y -> fst x == fst y) . sort
    lis = f lm
    tis = f tm
    isd = intersect (map fst lis) (map fst tis)
    g i = (i, x + y)
      where
        (Just x) = lookup i lis
        (Just y) = lookup i tis
        
gettd :: [String] -> ([(Int, Int)], [(Int, Int)])
gettd ss = (f lm, f (tail tm))
  where
    (lm, tm) = span (/= "") ss
    f = map $ (\[x,y] -> (x,y)) . map toInt . splitOn ","

toInt :: String -> Int
toInt s = read s
