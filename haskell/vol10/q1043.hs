import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (sortBy)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n (toRecord <$> getLine) >>= mapM_ print
    main

solve :: [Record] -> [Int]
solve rs = f 0 (V.replicate 1001 0) [] (sortRecord rs)
  where f :: Int -> Vector Int -> [Int] -> [Record] -> [Int]
        f _ _ as [] = reverse as
        f 26 _ as _ = reverse as
        f ct gv as (Record i u _ _ : rcs) | ct < 10 = if gv ! u < 3 then f (ct+1) (V.accum (+) gv [(u,1)]) (i:as) rcs
                                                      else f ct gv as rcs
                                          | ct < 20 = if gv ! u < 2 then f (ct+1) (V.accum (+) gv [(u,1)]) (i:as) rcs
                                                      else f ct gv as rcs
                                          | otherwise = if gv ! u < 1 then f (ct+1) (V.accum (+) gv [(u,1)]) (i:as) rcs
                                                      else f ct gv as rcs

data Record = Record Int Int Int Int deriving Show

sortRecord :: [Record] -> [Record]
sortRecord = sortBy f
  where f (Record i1 u1 a1 p1) (Record i2 u2 a2 p2) | a1 == a2 && p1 == p2 = compare i1 i2
                                                    | a1 == a2 = compare p1 p2
                                                    | otherwise = compare a2 a1

toRecord :: String -> Record
toRecord s = Record i u a p
  where [i,u,a,p] = map read . words $ s
