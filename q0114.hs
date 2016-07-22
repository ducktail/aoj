import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  xs <- getl $ split toInt
  when (sum xs /= 0) $ do
    print $ solve xs
    main

solve :: [Int] -> Int
solve [a1,m1,a2,m2,a3,m3] = lcm (lcm c1 c2) c3
  where
    c1 = rtnCnt 0 1 1 (move a1 m1)
    c2 = rtnCnt 0 1 1 (move a2 m2)
    c3 = rtnCnt 0 1 1 (move a3 m3)
    
move a m p = a * p `mod` m

rtnCnt :: Int -> Int -> Int -> (Int -> Int) -> Int
rtnCnt cnt from to f
  | nxt == to = cnt + 1
  | otherwise = rtnCnt (cnt + 1) nxt to f
  where
    nxt = f from
    
toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

tests :: [Test]
tests = [
  "move1" ~: 1 ~=? move 2 3 5,
  "rtnCnt1" ~: 1 ~=? rtnCnt 0 1 1 (move 12 11),
  "rtnCnt2" ~: 4 ~=? rtnCnt 0 1 1 (move 2 5),
  "solve1" ~: 12 ~=? solve [2,5,3,7,6,13],
  "solve2" ~: 116640000 ~=? solve [517, 1024, 746, 6561, 4303, 3125]
  ]
