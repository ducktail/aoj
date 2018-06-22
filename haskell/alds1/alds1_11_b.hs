import Control.Applicative ((<$>))
import Control.Monad (replicateM, forM_)
import Data.List (foldl', find)

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

main :: IO ()
main = do
  n <- readLn
  xss <- replicateM n (map read <$> words <$> getLine)
  solve n xss

solve :: Int -> [[Int]] -> IO ()
solve n xss = do
  printStat $ foldl' dfs (Stat n 1 (V.replicate n 0) (V.replicate n 0) (V.replicate n W)) [0 .. n-1]
  where gd = foldl' (\v (i:_:ls) -> v // [(i-1, map (subtract 1) ls)]) (V.replicate n []) xss
        dfs (Stat n tm gt bt cd) i | cd ! i == B = (Stat n tm gt bt cd)
                                   | otherwise = f (Stat n (tm+1) (gt // [(i,tm)]) bt (cd // [(i,G)])) [i]
        f st [] = st
        f (Stat n tm gt bt cd) (x:st) = case find (\k -> cd ! k == W) (gd ! x) of
                                         Just j -> f (Stat n (tm+1) (gt // [(j, tm)]) bt (cd // [(j,G)])) (j:x:st)
                                         Nothing -> f (Stat n (tm+1) gt (bt // [(x, tm)]) (cd // [(x,B)])) st

data Color = W | G | B deriving (Show, Eq, Ord)

data Stat = Stat Int Int (Vector Int) (Vector Int) (Vector Color) deriving (Show)

printStat :: Stat -> IO ()
printStat (Stat n _ gt bt _) = do
  forM_ [0 .. n-1] $ \i -> do
    putStrLn . unwords $ [show (i+1), show (gt ! i), show (bt ! i)]
