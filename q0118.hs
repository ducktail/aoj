import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  [h,w] <- getl $ split toInt
  when (h /= 0 || w /= 0) $ do
    xs <- replicateM h getLine
    print $ solve h w xs
    main
    
solve :: Int -> Int -> [String] -> Int
solve h w xs = fst $ foldl f (0,iarr) [(i,j) | i <- [1..h], j <- [1..w]]
  where
    iarr = listArray ((1,1),(h,w)) $ concat xs :: UArray (Int,Int) Char
    f (cnt, arr) p
      | arr ! p == ' ' = (cnt, arr)
      | otherwise = (cnt + 1, clrChar arr h w (arr ! p) p)

clrChar :: UArray (Int,Int) Char -> Int -> Int -> Char -> (Int, Int) -> UArray (Int,Int) Char
clrChar arr h w c (i,j)
  | arr ! (i,j) /= c = arr
  | otherwise = foldl f (arr // [((i,j),' ')]) lst
  where
    lst = [(u,v) | (u,v) <- [(i+1,j),(i-1,j),(i,j+1),(i,j-1)], u >= 1, u <= h, v >= 1, v <= w]
    f a p = clrChar a h w c p

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

tests :: [Test]
tests = [
  "clrChar1" ~: listArray ((1,1),(2,3)) "  @@  " ~=? clrChar (listArray ((1,1),(2,3)) "##@@##") 2 3 '#' (1,1),
  "clrChar2" ~: listArray ((1,1),(2,3)) "  @@  " ~=? clrChar (listArray ((1,1),(2,3)) "##@@##") 2 3 '#' (1,2),
  "clrChar3" ~: listArray ((1,1),(2,3)) "  @@  " ~=? clrChar (listArray ((1,1),(2,3)) "##@@##") 2 3 '#' (2,3),
  "solve1"   ~: 3 ~=? solve 2 3 ["##@","@##"],
  "solve2"   ~: 4 ~=? solve 3 4 ["##@*","@##*","@@**"]
  ]
