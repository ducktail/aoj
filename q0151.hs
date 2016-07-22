import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt)
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  when (n /= 0) $ do
    xs <- rgetl n $ map digitToInt
    print $ solve n xs
    main
    
solve :: Int -> [[Int]] -> Int
solve n xs = maximum [yoko, tate, nanamer, nanamel]
  where
    txs = transpose xs
    rxs = map reverse txs
    yoko = maximum . map maxSN $ xs
    tate = maximum . map maxSN $ txs
    nix = nanameIx n
    nanamel = maximum . map maxSN . map (map (\(i,j) -> xs !! i !! j)) $ nix
    nanamer = maximum . map maxSN . map (map (\(i,j) -> rxs !! i !! j)) $ nix
    
maxSN :: [Int] -> Int
maxSN = maximum . map (length . takeWhile (==1)) . tails

nanameIx :: Int -> [[(Int,Int)]]
nanameIx x = zipWith (\n ns -> [(i,n-i)| i <- ns]) [0..] (tail . inits $ [0..x-1]) ++ zipWith (\n ns -> [(i,n-i)| i <- ns]) [x..] (tails [1..x-1])

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine

tests :: [Test]
tests = [
  "test1" ~: 5 ~=? maxSN [0,1,0,0,1,1,0,0,1,1,1,0,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,0,1,1,1,1,1,0,0,0,1],
  "test2" ~: 0 ~=? maxSN [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  "test3" ~: 3 ~=? solve 3 [[0,1,0],[1,1,1],[0,0,1]],
  "test4" ~: 3 ~=? solve 3 [[1,0,0],[1,1,0],[1,0,0]],
  "test5" ~: 3 ~=? solve 3 [[1,0,1],[0,1,0],[0,0,1]],
  "test6" ~: 3 ~=? solve 3 [[0,0,1],[0,1,1],[1,0,0]]
  ]
