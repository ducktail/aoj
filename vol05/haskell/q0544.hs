import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM, unless)

import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

import Control.Monad.State

main :: IO ()
main = do
  [n, m] <- map read <$> words <$> getLine
  unless (n == 0 && m == 0) $ do
    solve n <$> replicateM n (read <$> getLine) <*> replicateM m (read <$> getLine) >>= print
    main

solve :: Int -> [Int] -> [Int] -> Int
solve n mp ds = evalState (f ds) (0, 0)
  where f :: [Int] -> State (Int, Int) Int
        f (x:xs) = do
          (p, ct) <- get
          let y = p + x
          if y >= n - 1 then return (ct + 1)
          else do
            let z = mpv ! y + y
            if z >= n - 1 then return (ct + 1)
            else do
              put (z, ct + 1)
              f xs
        mpv = V.fromList mp
