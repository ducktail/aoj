import Control.Applicative
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  [n, t] <- f
  solve t <$> replicateM n f >>= printf "%.10f\n"
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> Double
solve t = g . foldl f (0, 1)
  where
    f (mn, md) [d, n] = if n * md > d * mn then (n, d) else (mn, md)
    g (mn, md) = fromIntegral t * fromIntegral mn / (fromIntegral md)
