import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  tk <- g <$> f
  n <- readLn
  tas <- map g <$> replicateM n f
  m <- readLn
  tss <- reverse <$> map g <$> replicateM m f
  print $ solve tk tas tss
  where
    f = map read <$> words <$> getLine
    g [h, m] = h * 60 + m

solve :: Int -> [Int] -> [Int] -> Int
solve tk tas tss = maybe 0 (const 1) $ do
  t <- find (<= tk - 10) tss
  find (<= t - 11) tas
