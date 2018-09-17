import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (unfoldr)

main :: IO ()
main = do
  a <- readLn
  unless (a == 0) $ do
    solve a >>= print
    main

solve :: Int -> IO Int
solve = return . foldr f 0 . unfoldr g
  where g x | x == 0 = Nothing
            | otherwise = let (q, r) = divMod x 10 in Just (r, negate q)
        f x y = x + 10 * y
