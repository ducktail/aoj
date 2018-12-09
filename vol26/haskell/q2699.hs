import Control.Applicative
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  [d, e] <- f
  unless (d == 0 && e == 0) $ do
    printf "%f\n" $ solve d e
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Double
solve d e = f 200 (d, 0)
  where
    f :: Double -> (Int, Int) -> Double
    f dv (x, y)
      | x < y = dv
      | otherwise = f (min dv (abs $ fromIntegral e - (sqrt $ fromIntegral $ x ^ 2 + y ^ 2))) (x - 1, y + 1)
