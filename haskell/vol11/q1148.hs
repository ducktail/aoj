import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (foldl', nub)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM

main :: IO ()
main = do
  [n, m] <- map read <$> words <$> getLine
  unless (n == 0 && m == 0) $ do
    r <- readLn
    rs <- replicateM r ((\[t, _, m, s] -> Record t m s) <$> map read <$> words <$> getLine)
    q <- readLn
    qs <- replicateM q ((\[x,y,z] -> (x, y, z)) <$> map read <$> words <$> getLine)
    mapM_ print $ solve rs qs
    main

solve :: [Record] -> [(Int, Int, Int)] -> [Int]
solve rs qs = map (\(st, et, m) -> calct (st, et) (db ! m)) qs
  where db = IM.fromList $ map f (nub (map (\(_,_,m) -> m) qs))
        f x = (x, utm (filter (\(Record _ m _) -> m == x) rs))

data Record = Record Int Int Int deriving Show

utm :: [Record] -> [(Int, Int)]
utm = snd . foldl' f ([],[])
  where f (st, as) (Record t m s) | s == 1 = (t:st, as)
                                  | (null . tail) st = ([], (head st, t):as)
                                  | otherwise = (tail st, as)

calct :: (Int,Int) -> [(Int,Int)] -> Int
calct (st, et) = foldl' f 0
  where f tt (t1, t2) = tt + max 0 (min et t2 - max st t1)
