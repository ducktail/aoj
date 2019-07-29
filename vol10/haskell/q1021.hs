import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List

main :: IO ()
main = solve <$> f [] <*> g [] >>= putStrLn
  where
    f xs = do
      l <- getLine
      if l == "END_OF_TEXT"
        then return $ intercalate "\n" $ reverse xs
        else f (l:xs)
    g xs = do
      l <- getLine
      if l == "-"
        then return $ reverse xs
        else g (l:xs)
             
solve :: String -> [String] -> String
solve ts cs = evalState (f cs) ([], ts, [])
  where
    f :: [String] -> State (String, String, String) String
    f [] = do
      (as, bs, _) <- get
      return $ revAppend as bs
    f ("a":xs) = do
      (as, bs, cs) <- get
      let (a1s, a2s) = break (== '\n') as
      put (a2s, revAppend a1s bs, cs)
      f xs
    f ("e":xs) = do
      (as, bs, cs) <- get
      let (b1s, b2s) = break (== '\n') bs
      put (revAppend b1s as, b2s, cs)
      f xs
    f ("p":xs) = do
      (as, bs, cs) <- get
      let (a1s, a2s) = break (== '\n') as
      if null a2s
        then put ([], revAppend a1s bs, cs)
        else do
          let (a3s, a4s) = break (== '\n') (tail a2s)
          put (a4s, revAppend a3s ('\n' : (revAppend a1s bs)), cs)
      f xs
    f ("n":xs) = do
      (as, bs, cs) <- get
      let (b1s, b2s) = break (== '\n') bs
      if null b2s
        then do
          let (a1s, a2s) = break (== '\n') as
          put (a2s, revAppend a1s bs, cs)
        else put ('\n' : revAppend b1s as, tail b2s, cs)
      f xs
    f ("f":xs) = do
      (as, bs, cs) <- get
      if null bs
        then put (as, bs, cs)
        else put (head bs : as, tail bs, cs)
      f xs
    f ("b":xs) = do
      (as, bs, cs) <- get
      if null as
        then put (as, bs, cs)
        else put (tail as, head as : bs, cs)
      f xs
    f ("d":xs) = do
      (as, bs, cs) <- get
      if null bs
        then put (as, bs, cs)
        else put (as, tail bs, cs)
      f xs
    f ("k":xs) = do
      (as, bs, cs) <- get
      if null bs
        then put (as, bs, cs)
        else if head bs == '\n'
               then put (as, tail bs, "\n")
               else do
                 let (b1s, b2s) = break (== '\n') bs
                 put (as, b2s, b1s)
      f xs
    f ("y":xs) = do
      (as, bs, cs) <- get
      if null cs
        then put (as, bs, cs)
        else if cs == "\n"
               then put ('\n':as, bs, cs)
               else put (revAppend cs as, bs, cs)
      f xs

revAppend :: String -> String -> String
revAppend [] ys = ys
revAppend (x:xs) ys = revAppend xs (x:ys)
