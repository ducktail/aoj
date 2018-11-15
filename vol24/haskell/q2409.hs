import Control.Applicative
import Control.Monad
import Data.List
import Data.Function (on, fix)

main :: IO ()
main = do
  [n, m] <- f
  solve n <$> replicateM m ((\[a, b] -> (a, b)) <$> f) >>= putStrLn
  where
    f = map read <$> words <$> getLine

solve :: Int -> [(Int,Int)] -> String
solve n ps = f 0 1
  where
    f ct i
      | i > n = show ct
      | otherwise = let ps' = filter (\(a, b) -> a <= i && i <= b) ps
                    in if null ps'
                       then "Impossible"
                       else f (ct + 1) (1 + maximum (map snd ps'))
