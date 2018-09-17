import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n | null ys' = head zs - last xs
        | otherwise = 0
  where (xs, ys) = span (< n) ps
        (ys', zs) = break (> n) ys


isP :: Int -> Bool
isP x = all ((/= 0).(mod x)) . takeWhile ((<= x).(^ 2)) $ ps

ps :: [Int]
ps = 2 : unfoldr f 3
  where f x | isP x = Just (x, x+2)
            | otherwise = f (x+2)
