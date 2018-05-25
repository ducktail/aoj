import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (unfoldr)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n >>= print
    main

solve :: Int -> IO Int
solve = return . g . map tr . f
  where f = unfoldr (\x -> if x == 0 then Nothing
                           else let (q, r) = divMod x 8 in Just (r, q))
        tr x | x == 4 = 5
             | x > 4 = x + 2
             | otherwise = x
        g = foldr (\x y -> x + 10 * y) 0
