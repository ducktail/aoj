import Control.Applicative
import Control.Monad
import Data.Char (ord, chr)

main :: IO ()
main = do
  s <- getLine
  unless (s == ".") $ do
    putStrLn $ solve s
    main

solve :: String -> String
solve = f [] 0
  where
    f st _ [] = reverse st
    f st d (c:cs)
      | c == '[' = f (c:st) 0 cs
      | c == '+' = f st (d + 1) cs
      | c == '-' = f st (d - 1) cs
      | c == ']' = let (as, _:bs) = break (== '[') st
                   in f (reverse as ++ bs) 0 cs
      | otherwise = f (g d c : st) 0 cs
    g d c
      | c == '?' = 'A'
      | otherwise = let i = (ord c - ord 'A' + d) `mod` 26
                    in chr $ ord 'A' + i
