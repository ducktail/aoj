import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
  s <- getLine
  unless (s == "0") $ do
    putStrLn $ solve s
    main

solve :: String -> String
solve s = fst $ minimumBy (\x y -> dc c (snd x) `compare` dc c (snd y)) colors
  where
    c = toColor s

hexToInt :: String -> Int
hexToInt s = foldl (\a x -> a * 16 + x) 0 $ map digitToInt s

colors :: [(String,(Int,Int,Int))]
colors = [("black",(0,0,0)),("blue",(0,0,255)),
          ("lime",(0,255,0)),("aqua",(0,255,255)),
          ("red",(255,0,0)),("fuchsia",(255,0,255)),
          ("yellow",(255,255,0)),("white",(255,255,255))]

dc :: (Int,Int,Int) -> (Int,Int,Int) -> Int
dc (r,g,b) (rk,gk,bk) = (r - rk) ^ 2 + (g - gk) ^ 2 + (b - bk) ^ 2

toColor :: String -> (Int,Int,Int)
toColor s = (hexToInt r, hexToInt g, hexToInt b)
  where
    r = take 2 . tail $ s
    g = take 2 . drop 3 $ s
    b = drop 5 s
