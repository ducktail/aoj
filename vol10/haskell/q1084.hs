import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (unfoldr, foldl')

main :: IO ()
main = do
  [n, k] <- f
  unless (n == 0 && k == 0) $ do
    solve k <$> replicateM n (read <$> getLine) >>= print
    main
  where
    f = map read <$> words <$> getLine
    
solve :: Int -> [Int] -> Int
solve k xs = ck - cdk
  where
    ls = split3 k xs
    cdk = maximum $ map (\(_,ys,_) -> product ys) ls
    ck = foldl' f 0 ls
    f pt (ls, ms, rs) = let mn = minimum ms
                            mx = maximum (ls ++ rs)
                            pm = product ms
                        in maximum [pt, pm, pm `div` mn * mx]
    
split3 :: Int -> [Int] -> [([Int],[Int],[Int])]
split3 k xs = unfoldr f (Just ([],take k xs, drop k xs))
  where f Nothing = Nothing
        f (Just (ls, ms, [])) = Just ((ls, ms, []), Nothing)
        f (Just (ls, (m:ms), (r:rs))) = Just ((ls, (m:ms), (r:rs)), Just (m:ls, ms ++ [r], rs))
