import Control.Applicative
import Control.Monad
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n = let lv = n `div` 2 + 2
              v = V.unfoldr (\i -> if 2 * i <= n + 2
                                   then Just (i * (i + 1) `div` 2, i + 1)
                                   else Nothing) 0
          in length $ do
            i <- [0 .. lv - 2]
            guard $ bsearch v (v ! i + n) (i + 2) lv
            return ()
            

bsearch :: Vector Int -> Int -> Int -> Int -> Bool
bsearch v x l r
  | l >= r = False
  | x > y = bsearch v x (m + 1) r
  | x < y = bsearch v x l m
  | otherwise = True
  where
    m = (l + r) `div` 2
    y = v ! m
