import Control.Applicative ((<$>))
import Control.Monad (unless, forM_)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = loop pct
  where isP x = all ((/=0).(mod x)) $ takeWhile ((x>=).(^2)) (2:[3,5..])
        pct = V.create $ do
          v <- VM.replicate 246913 0
          forM_ [2..246912] $ \i -> do
            x <- VM.read v (i-1)
            if isP i then do
              VM.write v i (x+1)
            else do
              VM.write v i x
          return v
        loop pct = do
          n <- readLn
          unless (n == 0) $ do
            print $ solve pct n
            loop pct
          
solve :: Vector Int -> Int -> Int
solve pct n = pct ! (2 * n) - pct ! n
