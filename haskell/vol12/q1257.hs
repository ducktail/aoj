import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (unfoldr)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n = f 0 0 ls ls
  where ls = takeWhile (<= n) ps
        f ct sm [] [] = ct
        f ct sm (x:xs) [] | sm < n = ct
                          | sm == n = f (ct+1) (sm-x) xs []
                          | otherwise = f ct  (sm-x) xs []
        f ct sm (x:xs) (y:ys) | sm < n = f ct (sm+y) (x:xs) ys
                              | sm == n = f (ct+1) (sm-x) xs (y:ys)
                              | otherwise = f ct (sm-x) xs (y:ys)

ps :: [Int]
ps = 2 : unfoldr f 3
  where f x | x == 0 = Nothing
            | isP x = Just (x, x+2)
            | otherwise = f (x+2)

isP :: Int -> Bool
isP x = all ((/=0).(mod x)) . takeWhile ((<= x).(^2)) $ ps
