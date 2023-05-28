import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, l, r] <- f
  unless (n == 0 && l == 0 && r == 0) $ do
    solve n l r <$> replicateM n readLn >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Int -> [Int] -> Int
solve n l r as = foldl f 0 [l .. r]
  where
    f ct y = if leapYear n as y then ct + 1 else ct

leapYear n as x =
  case find ((== 0) . mod x . snd) (zip [1..] as) of
    Just (i, _) -> odd i
    Nothing -> even n
