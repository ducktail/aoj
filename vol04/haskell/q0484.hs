import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [_, t] <- f
  solve t <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [Int] -> Int
solve t = either trd trd . foldM f (True, 0, 0)
  where
    f (pr, st, ct) a
      | st + t <= a = return (True, a, ct + 1)
      | pr = return (False, st + t, ct)
      | otherwise = Left (False, st + t, ct)
    trd (_, _, x) = x
