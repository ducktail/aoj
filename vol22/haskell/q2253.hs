import Control.Applicative
import Control.Monad
import Data.List
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Sequence ((<|), (|>), (><), ViewL(..), ViewR(..), Seq)
import qualified Data.Sequence as Sq

main :: IO ()
main = do
  [t, n] <- f
  unless (t == 0 && n == 0) $ do
    solve t <$> replicateM n (g <$> f) <*> (g <$> f) >>= print
    main
  where
    f = map read <$> words <$> getLine
    g [x, y] = (x, y)

solve :: Int -> [(Int, Int)] -> (Int, Int) -> Int
solve t sps bp = let iv = V.replicate (123 ^ 2) (-1) // ((toIdx bp, 0) : [(i, -2)| i <- map toIdx sps]) :: Vector Int
                     iq = Sq.singleton bp
                 in loop iq iv 0
  where
    loop q v ct = case Sq.viewl q of
      EmptyL -> ct
      (x, y) :< rq -> let bst = v ! (toIdx (x, y))
                          nps = [(nx, ny) | bst < t,
                                            (dx, dy) <- [(1,0),(-1,0),(0,1),(0,-1),(1,1),(-1,-1)],
                                            let (nx, ny) = (x + dx, y + dy),
                                            v ! (toIdx (nx, ny)) == (-1)]
                          nq = foldl' (|>) rq nps
                          nv = v // [(i, bst+1) | i <- map toIdx nps]
                      in loop nq nv (ct + 1)

toIdx (x, y) = let (x', y') = (x + 61, y + 61)
               in x' + 123 * y'
