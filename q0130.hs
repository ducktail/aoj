import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Control.Monad.State as S
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n solve
  mapM_ putStrLn xs

solve :: String -> String
solve (s:ss) = S.evalState (parse ss) (0,[s])


parse :: String -> S.State (Int,String) String
parse [] = do
  (_,xs) <- S.get
  return xs

parse ('-':'>':c:cs) = do
  (p,xs) <- S.get
  if length xs == p + 1
    then do
      S.put (p+1,xs ++ [c])
      parse cs
    else do
      S.put (p+1,xs)
      parse cs

parse ('<':'-':c:cs) = do
  (p,xs) <- S.get
  if p == 0
    then do
      S.put (0,c:xs)
      parse cs
    else do
      S.put (p-1,xs)
      parse cs

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine


tests :: [Test]
tests = [
  "parse1" ~: "abcd" ~=? S.evalState (parse "->b->c->d") (0,"a"),
  "parse2" ~: "dcba" ~=? S.evalState (parse "<-b<-c<-d") (0,"a"),
  "parse3" ~: "bacde" ~=? S.evalState (parse "->a->c<-a->c->d<-c<-a<-b->a->c->d->e<-d") (0,"b"),
  "solve1" ~: "bcdae" ~=? solve "a->e<-a<-d->a->e<-a<-d<-c->d->a<-d<-c<-b->c->d<-c"
  ]
