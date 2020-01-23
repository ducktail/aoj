import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

eps :: Double
eps = 1e-9

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= print
    main
  where
    f = g <$> map read <$> splitOn "," <$> getLine
    g [x, y] = (x, y)

solve :: [(Double, Double)] -> Int
solve xs = foldl' f 1 iss
  where
    iss = concat $ do
      p0 <- xs
      p1 <- tail $ dropWhile (/= p0) xs
      guard $ inside 2.0 p0 p1
      return $ intersection p0 p1
    f mxc is = max mxc (foldl' g 0 xs)
      where
        g c p = if inside 1.0 is p then c + 1 else c 

inside :: Double -> (Double, Double) -> (Double, Double) -> Bool
inside sh (x0, y0) (x1, y1) = d < sh || eps >= abs (d - sh)
  where
    d = sqrt $ (x1 - x0) ^ 2 + (y1 - y0) ^ 2

intersection :: (Double, Double) -> (Double, Double) -> [(Double, Double)]
intersection (x0, y0) (x1, y1) = [(x0 + cos (t + a), y0 + sin (t + a)), (x0 + cos (t - a), y0 + sin (t - a))]
  where
    (v1x, v1y) = (x1 - x0, y1 - y0)
    d = sqrt $ v1x ^ 2 + v1y ^ 2
    a = acos (d / 2)
    t = atan2 v1y v1x
