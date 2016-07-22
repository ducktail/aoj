import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n $ split toInt
  mapM_ (putStrLn . unwords . map show) $ solve xs

solve :: [[Int]] -> [[Int]]
solve = map (minpath tbl)
  where
    tbl = [0,1,2,3,4,5,6,7,8,9,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,5,4,3,2,1]

minpath :: [Int] -> [Int] -> [Int]
minpath tbl [x,y] = take (iy - ix + 1) $ drop ix tbl
  where
    ixs = findIndices (== x) tbl
    iys = findIndices (== y) tbl
    (ix,iy) = snd $ minimumBy (compare `on` fst) [(j - i,(i,j)) | i <- ixs, j <- iys, j - i > 0, j - i < 15]

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
  "test1" ~: [2,3,4] ~=? minpath [0,1,2,3,4,5,6,7,8,9,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,5,4,3,2,1] [2,4],
  "test2" ~: [4,3,2] ~=? minpath [0,1,2,3,4,5,6,7,8,9,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,5,4,3,2,1] [4,2],
  "test3" ~: [8,9,5,4,3,2,1,0,1,2,3,4,5,6,7] ~=? minpath [0,1,2,3,4,5,6,7,8,9,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,5,4,3,2,1] [8,7],
  "test4" ~: [[2,3,4],[4,3,2]] ~=? solve [[2,4],[4,2]]
  ]
