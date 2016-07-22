import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array (range)
import qualified Data.IntMap as IM

main :: IO ()
main = do
  let tbl = mkTbl
  solve tbl

solve :: IM.IntMap (Int,Int,Int) -> IO ()
solve tbl = do
  x <- getl toInt
  when (x /= 0) $ do
    print . toPrice $ tbl IM.! x
    solve tbl

mkTbl :: IM.IntMap (Int,Int,Int)
mkTbl = foldl f IM.empty ilst
  where
    ilst = map (\(d,t,c) -> (200*d+300*t+500*c,(d,t,c))) . filter (\(d,t,c) -> 200*d+300*t+500*c <= 5000) $ range ((0,0,0),(25,16,10))
    f tbl (w,(d,t,c)) = IM.insertWith (\(pd,pt,pc) (cd,ct,cc) -> if toPrice (pd,pt,pc) < toPrice (cd,ct,cc) then (pd,pt,pc) else (cd,ct,cc)) w (d,t,c) tbl

toPrice :: (Int,Int,Int) -> Int
toPrice (d,t,c) = (d `div` 5) * 1520 + (d `mod` 5) * 380 + (t `div` 4) * 1870 + (t `mod` 4 ) * 550 + (c `div` 3) * 2244 + (c `mod` 3) * 850

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

-- mkTbl :: IM.IntMap (Int,Int,Int)
-- mkTbl = foldl f itbl [600,700..5000]
--   where
--     itbl = IM.fromList [(100,(100,100,100)), (200,(1,0,0)), (300,(0,1,0)), (400,(2,0,0)), (500,(0,0,1))]
--     f t x = IM.insert x (minimumBy (\a b -> toPrice a `compare` toPrice b) [incd (t IM.! (x - 200)), inct (t IM.! (x - 300)), incc (t IM.! (x - 500))])  t
--     incd (d,t,c) = (d+1,t,c)
--     inct (d,t,c) = (d,t+1,c)
--     incc (d,t,c) = (d,t,c+1)
