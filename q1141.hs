import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [a,d,n] <- getl $ wrds toInt
  unless (a == 0 && d == 0 && n == 0) $ do
    print $ solve a d n
    main

solve :: Int -> Int -> Int -> Int
solve a d n = filter isPn (toDList a d) !! (n-1)

toDList :: Int -> Int -> [Int]
toDList a d = iterate (+d) a

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

pns :: Integral a => [a]
pns = 2 : 3 : f [] (tail pns) 5
  where
    f ys (x:xs) n = ps ++ f (ys++[x]) xs (x^2+2)
      where
        ip m = all ((/= 0) . (mod m)) ys
        ps = filter ip [n, n+2 .. x^2-2]

isPn :: Integral a => a -> Bool
isPn 1 = False
isPn x = all ((/= 0) . mod x) $ takeWhile ((<= x) . (^2)) pns
