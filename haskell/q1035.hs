import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.Map as M
import Control.Monad.State
-- import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  [w,q] <- getl $ wrds toInt
  unless (w == 0 && q == 0) $ do
    xs <- replicateM q $ getl $ map toInt . tail . words
    solve w xs
    main

solve :: Int -> [[Int]] -> IO ()
solve w xs = do
  runStateT (mapM cat xs) ([(0,w)], M.empty)
  putStrLn "END"

cat :: [Int] -> StateT ([(Int,Int)], M.Map Int (Int,Int)) IO ()
cat [i, w] = do
  (sps, mp) <- get
  case findSp w sps of
   Just ((s,w),rs) -> do
     liftIO $ print s
     put (rs, M.insert i (s,w) mp)
   Nothing -> do
     liftIO $ putStrLn "impossible"
     put (sps,mp)

cat [i] = do
  (sps, mp) <- get
  put (addSp (mp M.! i) sps, M.delete i mp)

addSp :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
addSp (s,w) [] = [(s,w)]
addSp (s,w) ((ss,ww):sps)
  | s + w < ss = (s,w) : (ss,ww) : sps 
  | s + w == ss = (s,w + ww) : sps
  | ss + ww == s = addSp (ss, ww + w) sps
  | otherwise = (ss,ww) : addSp (s,w) sps

findSp :: Int -> [(Int,Int)] -> Maybe ((Int,Int), [(Int,Int)])
findSp w sps = case g sps of
                Just s -> Just ((s,w), f s sps)
                Nothing -> Nothing
  where
    f s ((ss,ww):rs)
      | s /= ss = (ss,ww) : f s rs
      | s == ss && w == ww = rs
      | otherwise = (ss+w, ww-w) : rs
    g [] = Nothing
    g ((ss,ww):rs)
      | w > ww = g rs
      | otherwise = Just ss
                    
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

-- tests :: [Test]
-- tests = [
--   "addSp1" ~: addSp (0,3) [] ~?= [(0,3)],
--   "addSp2" ~: addSp (0,3) [(5,1),(10,4)] ~?= [(0,3),(5,1),(10,4)],
--   "addSp3" ~: addSp (2,3) [(0,1),(10,4)] ~?= [(0,1),(2,3),(10,4)],
--   "addSp4" ~: addSp (10,2) [(0,3),(5,2)] ~?= [(0,3),(5,2),(10,2)],
--   "addSp5" ~: addSp (3,2) [(0,3),(10,2)] ~?= [(0,5),(10,2)],
--   "addSp6" ~: addSp (6,2) [(0,2),(4,1),(8,2),(15,4)] ~?= [(0,2),(4,1),(6,4),(15,4)],
--   "addSp7" ~: addSp (5,2) [(0,2),(4,1),(7,2),(15,4)] ~?= [(0,2),(4,5),(15,4)],
--   "findSp1" ~: findSp 5 [(0,2),(3,4),(10,3)] ~?= Nothing,
--   "findSp2" ~: findSp 5 [(0,2),(3,5),(10,7)] ~?= Just ((3,5),[(0,2),(10,7)]),
--   "findSp3" ~: findSp 7 [(0,2),(3,5),(10,20)] ~?= Just ((10,7),[(0,2),(3,5),(17,13)])
--   ]
