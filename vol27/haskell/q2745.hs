import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [r0, w0, c, r] <- f
  unless (r0 == 0 && w0 == 0 && c == 0 && r == 0) $ do
    print $ solve r0 w0 c r
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Int -> Int -> Int
solve r0 w0 c r
  | r0 >= w0 * c = 0
  | otherwise = ceil (c * w0 - r0) r
  where
    ceil x y = (x + y - 1) `div` y
