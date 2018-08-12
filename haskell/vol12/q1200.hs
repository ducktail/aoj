import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (foldl', unfoldr)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n = foldl' f 0 $ takeWhile ((<=n).(*2)) [2..]
  where f ct x | isP x && isP (n-x) = ct+1
               | otherwise = ct

ps = 2 : unfoldr f 3
  where f x | isP x = Just (x, x+2)
            | otherwise = f (x+2)

isP x = all ((/=0).(mod x)) . takeWhile ((<=x).(^2)) $ ps
