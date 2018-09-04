import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [m, nmin, nmax] <- f
  unless (m == 0 && nmin == 0 && nmax == 0) $ do
    solve nmin nmax <$> replicateM m readLn >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [Int] -> Int
solve nmin nmax ps = snd . maximum $ zip xs [nmin ..]
  where
    xs = take (nmax - nmin + 1) . drop (nmin - 1) $ zipWith (-) ps (tail ps)
