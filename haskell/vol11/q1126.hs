import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, forM_, when)
import Data.Char (isDigit, digitToInt)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  [w, h] <- map read <$> words <$> getLine
  unless (w == 0 && h == 0) $ do
    solve w h <$> replicateM h getLine >>= print
    main

solve :: Int -> Int -> [String] -> Integer
solve w h css = V.maximum dp
  where toIx (i, j) = i + (w+1) * j
        dp :: Vector Integer
        dp = V.create $ do
          v <- VM.replicate ((w+1)*(h+1)) 0
          forM_ [1..w] $ \i -> do
            forM_ [1..h] $ \j -> do
              let c = css !! (j-1) !! (i-1)
              when (isDigit c) $ do
                x <- VM.read v (toIx (i,j-1))
                y <- VM.read v (toIx (i-1,j))
                VM.write v (toIx (i,j)) (10 * (max x y) + (fromIntegral . digitToInt) c)
          return v
