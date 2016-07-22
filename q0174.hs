import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  s <- getLine
  unless (s == "0") $ do
    putStrLn $ solve s 
    solve <$> getLine >>= putStrLn
    solve <$> getLine >>= putStrLn
    main

solve :: String -> String
solve = unwords . map show . f . map length . group . sort . ("AB"++) . tail
  where
    f [x,y] = if x > y then [x,y-1] else [x-1,y]
