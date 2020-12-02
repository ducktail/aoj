import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  ns <- f
  when (sum ns > 0) $ do
    print $ solve ns
    main
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve ns = sum qs + m + f qs rs'
  where
    ns' = (zipWith (+) <$> take 3 <*> drop 3) ns
    qs = map (flip div 3) ns'
    rs = map (flip mod 3) ns'
    m = minimum rs
    rs' = map (subtract m) rs
    f [x, _, _] [0, 2, 2] = if x >= 1 then 1 else 0
    f [_, x, _] [2, 0, 2] = if x >= 1 then 1 else 0
    f [_, _, x] [2, 2, 0] = if x >= 1 then 1 else 0
    f _ _ = 0
