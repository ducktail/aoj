import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve n <$> replicateM n (getl toInt) >>= print
    main
    
solve :: Int -> [Int] -> Int
solve n xs = fst $ bsort n iar
  where
    iar = listArray (1,n) xs :: UArray Int Int

idxlst :: Int -> [Int]
idxlst m = foldl (\ls x -> ls ++ take x ls) [1..m-1] [m-2,m-3..1]

bsort :: Int -> UArray Int Int -> (Int, UArray Int Int)
bsort n ar = foldl f (0,ar) (idxlst n)
  where
    f (x,a) i
      | a ! i > a ! (i+1) = (x+1,a // [(i, a ! (i+1)),(i+1, a ! i)])
      | otherwise = (x,a)

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

tests :: [Test]
tests = [
  "test1" ~: [1,2,3,4,1,2,3,1,2,1] ~=? idxlst 5,
  "test2" ~: [1,2,1] ~=? idxlst 3,
  "test3" ~: [1] ~=? idxlst 2
  ]
