import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
-- import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    putStrLn $ solve n
    main

solve :: Int -> String
solve n = f 0 n
  where
    f c m
      | m == 0 = "NA"
      | m == 6174 = show c
      | otherwise = f (c+1) (op m)

op :: Int -> Int
op x = (frl . reverse) sl - frl sl
  where
    tol n = [n `div` 1000, n `mod` 1000 `div` 100, n `mod` 100 `div` 10, n `mod` 10]
    frl = foldl (\s n -> s * 10 + n) 0
    sl = sort $ tol x

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine

-- tests :: [Test]
-- tests = [
--   "op1" ~: op 2012 ~?= 2088,
--   "op2" ~: op 2088 ~?= 8532,
--   "op3" ~: op 8532 ~?= 6174
--   ]
