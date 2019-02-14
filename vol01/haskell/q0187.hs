import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  xs <- f
  unless (all (==0) xs) $ do
    solve (g xs) <$> (g <$> f) <*> (g <$> f) >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine :: IO [Int]
    g [x1, y1, x2, y2] = let v1 = Vect (fromIntegral x1) (fromIntegral y1)
                             v2 = Vect (fromIntegral x2) (fromIntegral y2)
                         in Segment v1 v2

solve :: Segment Double -> Segment Double -> Segment Double -> String
solve s1 s2 s3
  | not (is1 && is2 && is3) = "kyo"
  | eps > 1900000 - a = "dai-kichi"
  | eps > 1000000 - a = "chu-kichi"
  | eps > 100000 - a = "kichi"
  | eps < a = "syo-kichi"
  | otherwise = "kyo"
  where
    is1 = intersectSS s1 s2
    is2 = intersectSS s2 s3
    is3 = intersectSS s3 s1
    v1 = crossPoint s1 s2
    v2 = crossPoint s2 s3
    v3 = crossPoint s3 s1
    a = area v1 v2 v3

data Vect a = Vect {vecX :: a, vecY :: a} deriving Show
data Segment a = Segment {segB :: Vect a, segE :: Vect a} deriving Show
data PosRel = CCW | CW | OB | OF | OS deriving (Show, Read, Eq)

eps :: Floating a => a
eps = 1e-9

norm :: Num a => Vect a -> a
norm (Vect x y) = x ^ 2 + y ^ 2

dot :: Num a => Vect a -> Vect a -> a
dot (Vect x1 y1) (Vect x2 y2) = x1 * x2 + y1 * y2

cross :: Num a => Vect a -> Vect a -> a
cross (Vect x1 y1) (Vect x2 y2) = x1 * y2 - y1 * x2

(.+.) :: Num a => Vect a -> Vect a -> Vect a
(Vect x1 y1) .+. (Vect x2 y2) = Vect (x1 + x2) (y1 + y2)

(.-.) :: Num a => Vect a -> Vect a -> Vect a
(Vect x1 y1) .-. (Vect x2 y2) = Vect (x1 - x2) (y1 - y2)

(.*.) :: Num a => Vect a -> a -> Vect a
(Vect x y) .*. k = Vect (x * k) (y * k)

ccw :: (Ord a, Floating a) => Vect a -> Vect a -> Vect a -> PosRel
ccw v1 v2 v3
  | c > eps = CCW
  | c < negate eps = CW
  | dot v12 v13 < negate eps = OB
  | norm v12 < norm v13 = OF
  | otherwise = OS
  where
    v12 = v2 .-. v1
    v13 = v3 .-. v1
    c = cross v12 v13

intersectSS :: (Ord a, Floating a) => Segment a -> Segment a -> Bool
intersectSS s1 s2 = f s1 s2 && f s2 s1
  where
    f x y = case (ccw (segB x) (segE x) (segB y), ccw (segB x) (segE x) (segE y)) of
             (CCW, CW) -> True
             (CW, CCW) -> True
             (OS, CCW) -> True
             (OS, CW) -> True
             (CCW, OS) -> True
             (CW, OS) -> True
             _ -> False

crossPoint :: (Ord a, Floating a) => Segment a -> Segment a -> Vect a
crossPoint s1 s2 = segB s1 .+. ((segE s1 .-. segB s1) .*. t)
  where
    bv = segE s2 .-. segB s2
    d1 = abs $ cross bv $ segB s1 .-. segB s2
    d2 = abs $ cross bv $ segE s1 .-. segB s2
    t = d1 / (d1 + d2)

area :: (Floating a) => Vect a -> Vect a -> Vect a -> a
area v1 v2 v3 = 0.5 * abs (cross v12 v13)
  where
    v12 = v2 .-. v1
    v13 = v3 .-. v1
