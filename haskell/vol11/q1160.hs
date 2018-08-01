import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, when, guard, foldM)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

main :: IO ()
main = do
  [w, h] <- map read <$> words <$> getLine
  unless (w == 0 && h == 0) $ do
    solve w h <$> V.fromList <$> concat <$> replicateM h (map read <$> words <$> getLine) >>= print
    main

solve :: Int -> Int -> Vector Int -> Int
solve w h v = runST $ do
  mp <- V.thaw v
  foldM (f mp) 0 [(i, j) | i <- [0 .. w-1], j <- [0 .. h-1]]
  where toIx (i, j) = i + w * j
        f mp ct (i, j) = do
          x <- VM.read mp (toIx (i, j))
          if x == 0 then return ct
            else do
            clear mp (i, j)
            return $ ct + 1
        clear mp (i, j) = do
          x <- VM.read mp (toIx (i, j))
          when (x == 1) $ do
            VM.write mp (toIx (i, j)) 0
            mapM_ (clear mp) $ do
              (i', j') <- [(i+1,j),(i-1,j),(i,j+1),(i,j-1),(i-1,j-1),(i+1,j-1),(i-1,j+1),(i+1,j+1)]
              guard $ i' >= 0 && j' >= 0 && i' < w && j' < h
              return (i', j')
