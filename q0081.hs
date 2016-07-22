import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- getc $ map toDbl . splitOn ","
  mapM_ (putStrLn . unwords . map show) $ solve xs

solve :: [[Double]] -> [[Double]]
solve = map sympoint

sympoint :: [Double] -> [Double]
sympoint [x1,y1,x2,y2,xq,yq] = [xop pr, yop pr]
  where
    p1 = Point x1 y1
    p2 = Point x2 y2
    pq = Point xq yq
    pr = toPoint p1 vr
    va = toVect p1 p2
    vb = toVect p1 pq
    vr = addV vb vqr
    vqr = mulV 2 (subV (mulV (dot va vb / norm va) va) vb)
    
toDbl :: String -> Double
toDbl s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents

data Point = Point {xop :: Double, yop :: Double} deriving Show
data Vect = Vect {xov :: Double, yov :: Double} deriving Show
 
dot :: Vect -> Vect -> Double
dot v1 v2 = xov v1 * xov v2 + yov v1 * yov v2

norm :: Vect -> Double
norm v = (xov v) ^ 2 + (yov v) ^ 2

addV :: Vect -> Vect -> Vect
addV v1 v2 = Vect (xov v1 + xov v2) (yov v1 + yov v2)

subV :: Vect -> Vect -> Vect
subV v1 v2 = Vect (xov v1 - xov v2) (yov v1 - yov v2)

mulV :: Double -> Vect -> Vect
mulV k v = Vect (k * xov v) (k * yov v)

toVect :: Point -> Point -> Vect
toVect p1 p2 = Vect (xop p2 - xop p1) (yop p2 - yop p1)

toPoint :: Point -> Vect -> Point
toPoint p v = Point (xop p + xov v) (yop p + yov v)
