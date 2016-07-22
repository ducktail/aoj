import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  [yz,tz] <- getl $ split toInt
  when (yz /= 0 || tz /= 0) $ do
    n <- getl toInt
    xs <- getl $ split toInt
    putStrLn $ solve (yz,tz) xs
    main

solve :: (Int,Int) -> [Int] -> String
solve (yz,tz) xs
  | alvFrog [(yz,tz)] (mkzl xs) = "OK"
  | otherwise = "NA"

mkzl :: [Int] -> [(Int,Int)]
mkzl [] = []
mkzl (x:y:ys) = (x,y) : mkzl ys

mvFrog :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
mvFrog (yzf,tzf) (yzs,tzs) = intersect fzs szs
  where
    fzs = [(i,j) | (i,j) <- f (yzf,tzf), i >= 0, i <= 9,j >= 0, j <= 9]
    szs = [(i,j) | (i,j) <- g (yzs,tzs), i >= 0, i <= 9,j >= 0, j <= 9]
    f (i,j) = [(i-2,j),(i-2,j-1),(i-2,j+1),(i,j-2),(i-1,j-2),(i+1,j-2),(i,j+2),(i-1,j+2),(i+1,j+2),(i+2,j),(i+2,j-1),(i+2,j+1)]
    g (i,j) = [(i,j),(i-1,j),(i+1,j),(i,j-1),(i-1,j-1),(i+1,j-1),(i,j+1),(i-1,j+1),(i+1,j+1)]

alvFrog :: [(Int,Int)] -> [(Int,Int)] -> Bool
alvFrog xs [] = (not . null) xs
alvFrog xs (s:ss) = alvFrog (nub $ concatMap (flip mvFrog s) xs) ss

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine


tests :: [Test]
tests = [
  "mkzl1" ~: [(1,2),(3,4),(5,6)] ~=? mkzl [1,2,3,4,5,6],
  "mvFrog1" ~: [(6,3),(5,3),(7,3)] ~=? mvFrog (6,1) (6,4),
  "mvFrog2" ~: [(5,3)] ~=? mvFrog (6,1) (4,4),
  "mvFrog3" ~: [] ~=? mvFrog (6,1) (3,4),
  "alvFrog1" ~: True ~=? alvFrog [(6,1)] [(6,4),(3,3)],
  "alvFrog2" ~: False ~=? alvFrog [(6,1)] [(6,4),(3,3),(1,8),(5,9),(7,7)],
  "solve1" ~: "OK" ~=? solve (6,1) [6, 4, 3, 3, 1, 2, 0, 5, 4, 6, 1, 8, 5, 9, 7, 7, 8, 6, 8, 3],
  "solve2" ~: "NA" ~=? solve (6,1) [6, 4, 3, 3, 1, 2, 0, 5, 4, 6, 1, 8, 5, 9, 7, 7, 8, 6, 9, 0]
  ]
