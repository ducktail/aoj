import Control.Applicative
import Control.Monad
import Data.List
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  [t, p, r] <- g
  unless (t == 0 && p == 0 && r == 0) $ do
    solve t <$> replicateM r f >>= mapM_ putStrLn
    main
  where
    f = words <$> getLine
    g = map read <$> f

solve :: Int -> [[String]] -> [String]
solve t xss = let idb = V.replicate t (0, 0) :: Vector (Int, Int)
                  ice = M.empty :: Map (Int, Int) Int
                  isc = S.empty :: Set (Int, Int)
                  (db, _, _) = foldl' f (idb, ice, isc) xss
              in map (\((a,b),c) -> (unwords . map show) [c, a, b]) $ sortBy g $ zip (V.toList db) [1..]
  where
    f (db, ce, sc) [sti, spi, stm, mes] =
      let (ti, pi, tm) = (read sti - 1, read spi, read stm) :: (Int, Int, Int)
      in if mes == "CORRECT"
           then
             if S.member (ti, pi) sc
               then (db, ce, sc)
               else let (c, p) = db ! ti
                        e = M.findWithDefault 0 (ti, pi) ce
                    in (db // [(ti, (c + 1, p + tm + 1200 * e))], ce, S.insert (ti, pi) sc)
           else
             let e = M.findWithDefault 0 (ti, pi) ce
             in (db, M.insert (ti, pi) (e + 1) ce, sc)
    g ((ca, pa), ia) ((cb, pb), ib)
      | ca == cb && pa == pb = compare ia ib
      | ca == cb = compare pa pb
      | otherwise = compare cb ca
