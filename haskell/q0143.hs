import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n $ split toDbl
  mapM_ putStrLn $ map solve xs

solve :: [Double] -> String
solve [xp1,yp1,xp2,yp2,xp3,yp3,xk,yk,xs,ys]
  | inside (p1,p2,p3) pk `xor` inside (p1,p2,p3) ps = "OK"
  | otherwise = "NG"
  where
    p1 = Point xp1 yp1
    p2 = Point xp2 yp2
    p3 = Point xp3 yp3
    pk = Point xk yk
    ps = Point xs ys

inside :: (Point, Point, Point) -> Point -> Bool
inside (p1, p2, p3) p = all (> 0) [c1,c2,c3] || all (< 0) [c1,c2,c3]
  where
    c1 = cross (toVect p1 p2) (toVect p1 p)
    c2 = cross (toVect p2 p3) (toVect p2 p)
    c3 = cross (toVect p3 p1) (toVect p3 p)

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False
    
toInt :: String -> Int
toInt s = read s

toDbl :: String -> Double
toDbl s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine


data Point = Point {xop :: Double, yop :: Double} deriving Show
data Vect = Vect {xov :: Double, yov :: Double} deriving Show

cross :: Vect -> Vect -> Double
cross v1 v2 = xov v1 * yov v2 - yov v1 * xov v2

toVect :: Point -> Point -> Vect
toVect p1 p2 = Vect (xop p2 - xop p1) (yop p2 - yop p1)


tests :: [Test]
tests = [
  "test1" ~: True ~=? inside ((Point 10 1),(Point 10 10),(Point 1 10)) (Point 9 9),
  "test2" ~: False ~=? inside ((Point 10 1),(Point 10 10),(Point 1 10)) (Point 1 1),
  "test3" ~: "OK" ~=? solve [10, 1, 10, 10, 1, 10, 9, 9, 1, 1],
  "test3" ~: "NG" ~=? solve [10, 1, 10, 10, 1, 10, 9, 9, 8, 8]
  ]
