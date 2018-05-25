import Control.Applicative ((<$>))
import Control.Monad (unless)

main :: IO ()
main = do
  b1 <- getLine
  unless (b1 == "0") $ do
    b2 <- getLine
    b3 <- getLine
    solve b1 b2 b3 >>= putStrLn
    main

solve :: String -> String -> String -> IO String
solve [b11, b12, b13] [b21, b22, b23] [b31, b32, b33] = return . maybe "NA" id . foldr g Nothing $ lst
  where f xs = and (zipWith (==) xs (tail xs)) && '+' `notElem` xs
        g xs r = if f xs then Just ([head xs]) else r
        lst = [[b11, b12, b13],
               [b21, b22, b23],
               [b31, b32, b33],
               [b11, b21, b31],
               [b12, b22, b32],
               [b13, b23, b33],
               [b11, b22, b33],
               [b13, b22, b31]]
