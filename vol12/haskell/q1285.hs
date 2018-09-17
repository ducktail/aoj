import Control.Applicative
import Control.Monad
import Data.List
import Data.Ratio
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Text.Printf

main :: IO ()
main = do
  [n, w] <- map read <$> words <$> getLine
  unless (n == 0 && w == 0) $ do
    solve w <$> (replicateM n readLn) >>= printf "%f\n"
    main

solve :: Int -> [Int] -> Double
solve w vs = fr $ IM.foldlWithKey g (1 % 100) hg
  where hg = foldl' f IM.empty vs :: IntMap Int
        f m v = IM.insertWith (+) (v `div` w) 1 m
        mk = fst . IM.findMax $ hg
        mv = IM.foldl' max 0 hg
        g s k v = s + ((mk - k) % mk) * (v % mv)
        fr x = (fromIntegral (numerator x)) / (fromIntegral (denominator x))
