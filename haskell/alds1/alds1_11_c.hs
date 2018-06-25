import Control.Applicative ((<$>))
import Control.Monad (replicateM, forM_)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Sequence ((<|), (|>), (><), ViewL(..), ViewR(..), Seq)
import qualified Data.Sequence as Sq
import Control.Monad.State

main :: IO ()
main = do
  n <- readLn
  solve n <$> replicateM n (map read <$> words <$> getLine) >>= printDist n
  -- solve n <$> replicateM n (map read <$> words <$> getLine) >>= printDist

solve :: Int -> [[Int]] -> Vector Int
solve n xss = evalState (bfs (Sq.singleton 0)) (Stat dd dc)
  where gd = foldl' (\v (i:_:ls) -> v // [(i-1, map (subtract 1) ls)]) (V.replicate n []) xss
        dd = V.replicate n (-1) // [(0, 0)]
        dc = V.replicate n W // [(0, B)]
        bfs :: Seq Int -> State Stat (Vector Int)
        bfs que = do
          case Sq.viewl que of
           EmptyL -> do
             Stat d _ <- get
             return d
           (u :< que') -> do
             Stat d col <- get
             let nbl = (filter ((==W) . (col !)) (gd ! u))
             let (d', col', que'') = foldl' (\(td, tc, tq) v -> (td // [(v, (td ! u) + 1)],
                                                          tc // [(v, B)],
                                                          tq |> v)) (d, col, que') nbl
             put (Stat d' col')
             bfs que''
             
data Color = W | B deriving (Show, Eq, Ord)

data Stat = Stat (Vector Int) (Vector Color) deriving (Show)

-- printDist :: Vector Int -> IO ()
-- printDist v = V.imapM_ (\i x -> putStrLn . unwords . map show $ [i+1, x]) v

printDist :: Int -> Vector Int -> IO ()
printDist n v = do
  forM_ [0..n-1] $ \i -> do
    putStrLn . unwords . map show $ [i+1, v ! i]
