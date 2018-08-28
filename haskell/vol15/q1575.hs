import Control.Applicative
import Data.List
import Data.Bool (bool)

main :: IO ()
main = solve <$> f <*> (getLine >> f) >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> Int
solve [w, h] = fst . foldl' f (0, (0,0))
  where
    f (ct, (o, i)) p
      | co < w-1 = (ct + bool 0 1 (co == ci), (co, ci))
      | co == w-1 = (ct, (co, ci))
      | co < w+h-2 = (ct + bool 0 1 (co-2 == ci), (co, ci))
      | co == w+h-2 = (ct, (co, ci))
      | otherwise = (ct + bool 0 1 (co-4 == ci), (co, ci))
      where
        (co, ci) = if p == 0 then (o+1, i) else (o, i+1)
