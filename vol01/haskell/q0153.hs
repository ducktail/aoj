import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [x1, y1] <- f
  unless (x1 == 0 && y1 == 0) $ do
    [x2, y2] <- f
    [x3, y3] <- f
    [xc, yc] <- f
    r <- readLn
    let v1 = Vect (fromIntegral x1) (fromIntegral y1)
    let v2 = Vect (fromIntegral x2) (fromIntegral y2)
    let v3 = Vect (fromIntegral x3) (fromIntegral y3)
    let vc = Vect (fromIntegral xc) (fromIntegral yc)
    putStrLn $ solve (Triangle v1 v2 v3) (Circle vc (fromIntegral r))
    main
  where
    f = map read <$> words <$> getLine

solve :: Triangle Double -> Circle Double -> String
solve tri cir
  | triInCir tri cir = "b"
  | cirInTri tri cir = "a"
  | cirOutTri tri cir = "d"
  | otherwise = "c"

eps :: Floating a => a
eps = 1e-9

data Vect a = Vect {vecX :: a, vecY :: a} deriving Show
data Segment a = Segment {segB :: Vect a, segE :: Vect a} deriving Show
data Circle a = Circle {cirC :: Vect a, cirR :: a} deriving Show
data Triangle a = Triangle {triA :: Vect a, triB :: Vect a, triC :: Vect a} deriving Show

norm :: Num a => Vect a -> a
norm (Vect x y) = x ^ 2 + y ^ 2

(.-.) :: Num a => Vect a -> Vect a -> Vect a
(Vect x1 y1) .-. (Vect x2 y2) = Vect (x1 - x2) (y1 - y2)

cross :: Num a => Vect a -> Vect a -> a
cross (Vect x1 y1) (Vect x2 y2) = x1 * y2 - y1 * x2

dot :: Num a => Vect a -> Vect a -> a
dot (Vect x1 y1) (Vect x2 y2) = x1 * x2 + y1 * y2

absV :: Floating a => Vect a -> a
absV = sqrt . norm

distanceLP :: (Floating a) => Segment a -> Vect a -> a
distanceLP (Segment v1 v2) v = abs (cross base (v .-. v1) / absV base)
  where
    base = v2 .-. v1

distanceSP :: (Ord a, Floating a) => Segment a -> Vect a -> a
distanceSP (Segment v1 v2) v
  | dot (v2 .-. v1) (v .-. v1) < 0 = absV (v .-. v1)
  | dot (v1 .-. v2) (v .-. v2) < 0 = absV (v .-. v2)
  | otherwise = distanceLP (Segment v1 v2) v

triInCir :: Triangle Double -> Circle Double -> Bool
triInCir tri cir = (abs (r2 - nca) < eps || r2 > nca) &&
                   (abs (r2 - ncb) < eps || r2 > ncb) &&
                   (abs (r2 - ncc) < eps || r2 > ncc)
  where
    nca = norm ((triA tri) .-. (cirC cir))
    ncb = norm ((triB tri) .-. (cirC cir))
    ncc = norm ((triC tri) .-. (cirC cir))
    r2  = (cirR cir) ^ 2

ccInTri :: Triangle Double -> Circle Double -> Bool
ccInTri tri cir = ((abs ca < eps || ca > 0) &&
                   (abs cb < eps || cb > 0) &&
                   (abs cc < eps || cc > 0)) ||
                  ((abs ca < eps || ca < 0) &&
                   (abs cb < eps || cb < 0) &&
                   (abs cc < eps || cc < 0))
  where
    ca = cross ((triB tri) .-. (triA tri)) ((cirC cir) .-. (triA tri))
    cb = cross ((triC tri) .-. (triB tri)) ((cirC cir) .-. (triB tri))
    cc = cross ((triA tri) .-. (triC tri)) ((cirC cir) .-. (triC tri))

cirInTri :: Triangle Double -> Circle Double -> Bool
cirInTri tri cir = ccInTri tri cir &&
                   (abs (la - (cirR cir)) < eps || la > (cirR cir)) && 
                   (abs (lb - (cirR cir)) < eps || lb > (cirR cir)) && 
                   (abs (lc - (cirR cir)) < eps || lc > (cirR cir))
  where
    la = distanceLP (Segment (triB tri) (triC tri)) (cirC cir)
    lb = distanceLP (Segment (triC tri) (triA tri)) (cirC cir)
    lc = distanceLP (Segment (triA tri) (triB tri)) (cirC cir)
    
cirOutTri :: Triangle Double -> Circle Double -> Bool
cirOutTri tri cir = not (ccInTri tri cir) &&
                    la > (cirR cir) &&
                    lb > (cirR cir) &&
                    lc > (cirR cir)
  where
    la = distanceSP (Segment (triB tri) (triC tri)) (cirC cir)
    lb = distanceSP (Segment (triC tri) (triA tri)) (cirC cir)
    lc = distanceSP (Segment (triA tri) (triB tri)) (cirC cir)
