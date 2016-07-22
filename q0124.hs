import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n words
  mapM_ putStrLn $ scoreSheet xs
  solve
  
solve :: IO ()
solve = do
  n <- getl toInt
  when (n /= 0) $ do
    xs <- rgetl n words
    putStrLn ""
    mapM_ putStrLn $ scoreSheet xs
    solve
    
scoreSheet :: [[String]] -> [String]
scoreSheet xs = map (\(p, s) -> s ++ "," ++ show p) ys
  where
    ys = sortBy (\a b -> fst b `compare` fst a) $ map calcScore xs

calcScore :: [String] -> (Int,String)
calcScore [nm,sk,_,sh] = (3*k+h,nm)
  where
    k = toInt sk
    h = toInt sh

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine


tests :: [Test]
tests = [
  "calcScore1" ~: (5,"Japan") ~=? calcScore ["Japan", "1", "0", "2"],
  "calcScore2" ~: (3,"Egypt") ~=? calcScore ["Egypt", "1", "2", "0"],
  "scoreSheet1" ~: ["Japan,5","Egypt,3","Canada,1"] ~=? scoreSheet [["Canada", "0", "2", "1"],["Egypt", "1", "2", "0"],["Japan", "1", "0", "2"]]
  ]
