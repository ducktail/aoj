import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Control.Monad.State

main :: IO ()
main = do
  n <- getl toInt
  es <- lines <$> getContents
  mapM_ print . take n . solve $ es

solve :: [String] -> [Int]
solve es = evalState (bbsim es) ([],0,0,0,0,0)

bbsim :: [String] -> State ([Int],Int,Int,Int,Int,Int) [Int]
bbsim [] = do
  (ps,_,_,_,_,_) <- get
  return . reverse $ ps

bbsim (s:ss)
  | s == "HIT" = do
      (ps,p,o,fb,sb,tb) <- get
      put (ps,p + tb,o,1,fb,sb)
      bbsim ss
  | s == "HOMERUN" = do
      (ps,p,o,fb,sb,tb) <- get
      put (ps,p + tb + sb + fb + 1,o,0,0,0)
      bbsim ss
  | s == "OUT" = do
      (ps,p,o,fb,sb,tb) <- get
      if o == 2 then put (p:ps,0,0,0,0,0)
        else put (ps,p,o+1,fb,sb,tb)
      bbsim ss

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
