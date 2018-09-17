import Control.Applicative ((<$>))
import Control.Monad (replicateM, forM_, foldM)
import Data.List (foldl', find)

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import Control.Monad.State

main :: IO ()
main = do
  n <- readLn
  xss <- replicateM n (map read <$> words <$> getLine)
  solve n xss >>= printStat n

solve :: Int -> [[Int]] -> IO Stat
solve n xss = foldM f (Stat 1 (V.replicate n 0) (V.replicate n 0) (V.replicate n W)) [0 .. n-1]
  where gd = foldl' (\v (i:_:ls) -> v // [(i-1, map (subtract 1) ls)]) (V.replicate n []) xss
        f st@(Stat tm gt bt cd) i | cd ! i == B = return st
                                  | otherwise = return $ evalState (dfs [i]) (Stat (tm+1) (gt // [(i, tm)]) bt (cd // [(i,G)]))
        dfs :: [Int] -> State Stat Stat
        dfs [] = get
        dfs (u:st) = do
          (Stat tm gt bt cd) <- get
          case find (\v -> cd ! v == W) (gd ! u) of
           Just v -> do
             put $ Stat (tm+1) (gt // [(v, tm)]) bt (cd // [(v,G)])
             dfs (v:u:st)
           Nothing -> do
             put $ Stat (tm+1) gt (bt // [(u, tm)]) (cd // [(u,B)])
             dfs st

data Color = W | G | B deriving (Show, Eq, Ord)

data Stat = Stat Int (Vector Int) (Vector Int) (Vector Color) deriving (Show)

printStat :: Int -> Stat -> IO ()
printStat n (Stat _ gt bt _) = do
  forM_ [0 .. n-1] $ \i -> do
    putStrLn . unwords $ [show (i+1), show (gt ! i), show (bt ! i)]
