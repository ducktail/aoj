import Control.Applicative ((<$>))
import Control.Monad (unless)

main :: IO ()
main = do
  s <- getLine
  unless (s == ".") $ do
    putStrLn $ solve s
    main

solve :: String -> String
solve s = f [] s
  where f st [] = if null st then "yes" else "no"
        f st ('(':rs) = f ('(': st) rs
        f st ('[':rs) = f ('[': st) rs
        f (x:xs) (')':rs) = if x == '(' then f xs rs else "no"
        f (x:xs) (']':rs) = if x == '[' then f xs rs else "no"
        f [] (')':rs) = "no"
        f [] (']':rs) = "no"
        f st (_:rs) = f st rs
