import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

main :: IO ()
main = do
  [n, p] <- map read <$> words <$> getLine
  unless (n == 0 && p == 0) $ do
    print $ solve n p
    main

solve :: Int -> Int -> Int
solve n p = runST $ do
  stn <- VM.replicate n 0
  step 0 p stn
  where step :: Int -> Int -> STVector s Int -> ST s Int
        step trn bwl stn = do
          if bwl > 1 then do
            modify stn (+1) trn
            step ((trn+1) `mod` n) (bwl-1) stn
          else if bwl == 1 then do
            x <- VM.read stn trn
            if x+1 == p then do
              return trn
            else do
              VM.write stn trn (x+1)
              step ((trn+1) `mod` n) 0 stn
          else do
            x <- VM.read stn trn
            VM.write stn trn 0
            step ((trn+1) `mod` n) x stn

modify :: STVector s Int -> (Int->Int) -> Int -> ST s ()
modify v f i = do
  x <- VM.read v i
  VM.write v i (f x)
