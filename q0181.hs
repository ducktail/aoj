import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [m,n] <- getl $ wrds toInt
  unless (m == 0 && n == 0) $ do
    solve m <$> replicateM n (getl toInt) >>= print
    main
    
solve :: Int -> [Int] -> Int
solve m xs = search m xs 0 (sum xs)

search :: Int -> [Int] -> Int -> Int -> Int
search m xs nw gw
  | nw + 1 == gw = gw
  | isStore w m xs = search m xs nw w
  | otherwise = search m xs w gw
  where
    w = (nw + gw) `div` 2

isStore :: Int -> Int -> [Int] -> Bool
isStore w 0 xs
  | null xs = True
  | otherwise = False
isStore w m xs = isStore w (m-1) ys
  where
    ys = store w 0 xs

store :: Int -> Int -> [Int] -> [Int]
store w l [] = []
store w l (x:xs)
  | w >= (l+x) = store w (l+x) xs
  | otherwise = (x:xs)

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
