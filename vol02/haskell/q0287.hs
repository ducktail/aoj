import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [m, n] <- f
  unless (m == 0 && n == 0) $ do
    putStrLn $ solve m n
    main
  where
    f = map read <$> words <$> getLine

solve :: Integer -> Integer -> String
solve m n = concat $ reverse $ zipWith (\x s -> if x == 0 then "" else show x ++ s) ls us
  where
    us = ["", "Man", "Oku", "Cho", "Kei", "Gai", "Jo", "Jou", "Ko",
          "Kan", "Sei", "Sai", "Gok", "Ggs", "Asg", "Nyt", "Fks", "Mts"]
    ls = unfoldr (\x -> if x == 0 then Nothing else let (q, r) = divMod x 10000 in Just (r, q)) $ expt m n

expt :: Integer -> Integer -> Integer
expt m n
  | n == 1 = m
  | even n = x * x
  | otherwise = x * x * m
  where
    x = expt m (n `div` 2)
