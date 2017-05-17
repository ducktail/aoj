import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = mapM_ solve =<< (getc $ split toInt)

solve :: [Int] -> IO ()
solve [p,q] = case c of
  Just p -> putStrLn a >> putStrLn p
  Nothing -> putStrLn a
  where
    (a,c) = divNum [] [p] p q
    
divNum :: [Int] -> [Int] -> Int -> Int -> (String, Maybe String)
divNum as rs p q
  | m == 0 = ((concatMap show . reverse) (d:as), Nothing)
  | m `elem` rs = ((concatMap show . reverse) (d:as), Just ( replicate (length as - idx) ' ' ++ replicate (idx+1) '^'))
  | otherwise = divNum (d:as) (m:rs) m q
  where
    (d,m) = divMod (10 * p) q
    idx = maybe 0 id . findIndex (== m) $ rs
    
toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents

tests :: [Test]
tests = [
  "divNum1" ~: ("5", Nothing) ~=? divNum [] [] 1 2,
  "divNum2" ~: ("30517578125", Nothing) ~=? divNum [] [] 10000 32768,
  "divNum3" ~: ("083", Just "  ^") ~=? divNum [] [] 1 12,
  "divNum4" ~: ("00009", Just "  ^^^") ~=? divNum [] [] 1 11100
  ]
