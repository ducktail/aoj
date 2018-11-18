import Control.Applicative
import Control.Monad
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

main :: IO ()
main = do
  [n, t, _, l] <- f
  solve n t l <$> (V.fromList <$> replicateM n (g <$> f)) >>= print
  where
    f = map read <$> words <$> getLine
    g [a, b] = (a, b)

solve :: Int -> Int -> Int -> Vector (Int, Int) -> Int
solve n t l dv = f 0 t 0 dv
  where
    f i ts rm dv =
      case dv ! i of
       (0, 0) -> i + 1
       (0, hi) -> let ch = rm + 1
                  in if ts < ch
                    then i + 1
                     else f ((i + 1) `mod` n) (ts - ch) 0 (dv // [(i, (ch, hi - 1))])
       (ti, hi) -> let rm' = rm + 1
                       ts' = ts + 1
                       ch = rm' - 9
                   in if ts' > l
                      then i + 1
                      else if rm' >= 9
                           then if ts' < ch
                                then i + 1
                                else f ((i + 1) `mod` n) (ts' - ch) 0 (dv // [(i, (ti - 1 + ch, hi))])
                           else f ((i + 1) `mod` n) ts' rm' (dv // [(i, (ti - 1, hi))])
