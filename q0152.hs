import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  when (n /= 0) $ do
    xs <- rgetl n $ split toInt
    mapM_ (\(i,n) -> putStrLn $ show i ++ " " ++ show n) $ solve xs
    main
    
solve :: [[Int]] -> [(Int,Int)]
solve = sortBy (\a b -> if snd a == snd b then fst a `compare` fst b else snd b `compare` snd a) . map (f (calc 0 1))
  where
    f g (y:ys)= (y, g ys)


calc :: Int -> Int -> [Int] -> Int
calc acc 10 (x:y:zs)
  | x == 10 = acc + x + y + head zs
  | x + y == 10 = acc + x + y + head zs
  | otherwise = acc + x + y
calc acc t (x:y:z:zs)
  | x == 10 = calc (acc + x + y + z) (t+1) (y:z:zs)
  | x + y == 10 = calc (acc + x + y + z) (t+1) (z:zs)
  | otherwise = calc (acc + x + y) (t+1) (z:zs)

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine


tests :: [Test]
tests = [
  "test1" ~: 273 ~=? calc 270 10 [1,2],
  "test2" ~: 285 ~=? calc 270 10 [1,9,5],
  "test3" ~: 300 ~=? calc 270 10 [10,10,10],
  "test4" ~: 123 ~=? calc 0 1 [6, 3, 10, 7, 1, 0, 7, 9, 1, 10, 6, 2, 4, 3, 9, 1, 9, 0],
  "test5" ~: 127 ~=? calc 0 1 [5, 3, 9, 1, 7, 1, 0, 0, 8, 1, 10, 10, 4, 3, 9, 1, 8, 2, 9],
  "test6" ~: 60 ~=? calc 0 1 [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
  "test7" ~: 200 ~=? calc 0 1 [8, 2, 10, 9, 1, 7, 0, 10, 10, 10, 0, 8, 10, 10, 10, 10],
  "test8" ~: 122 ~=? calc 0 1 [5, 0, 10, 9, 1, 4, 1, 9, 0, 10, 10, 7, 1, 5, 2, 8, 1],
  "test9" ~: 300 ~=? calc 0 1 [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10],
  "test10" ~: 175 ~=? calc 0 1 [8, 2, 7, 3, 6, 4, 8, 2, 8, 2, 9, 1, 7, 3, 6, 4, 8, 2, 9, 1, 7]
  ]
