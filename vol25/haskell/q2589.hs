import Control.Applicative
import Control.Monad
import Data.Ratio

main :: IO ()
main = do
  s <- getLine
  unless (s == "#") $ do
    prt $ solve s
    main

solve :: String -> Rational
solve = snd . foldr f (0, 0) . filter (\c -> c == 'w' || c == 'n')
  where
    f 'w' (0, _) = (1, 90)
    f 'n' (0, _) = (1, 0)
    f 'w' (i, a) = (i + 1, a + 90 % (2 ^ i))
    f 'n' (i, a) = (i + 1, a - 90 % (2 ^ i))

prt :: Rational -> IO ()
prt r
  | denominator r == 1 = print $ numerator r
  | otherwise = putStrLn $ show (numerator r) ++ "/" ++ show (denominator r)
