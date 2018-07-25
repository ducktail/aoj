import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.State

main :: IO ()
main = do
  [w, h] <- map read <$> words <$> getLine
  unless (w == 0 && h == 0) $ do
    solve w h >>= printPos
    main

solve :: Int -> Int -> IO Status
solve w h = evalStateT (walk w h) ((1, 1), N)

walk :: Int -> Int -> StateT Status IO Status
walk w h = do
  st <- get
  cs <- lift $ words <$> getLine
  case cs of
   ["FORWARD", sd] -> do
     put $ forward w h (read sd) st
     walk w h
   ["BACKWARD", sd] -> do
     put $ backward w h (read sd) st
     walk w h
   ["RIGHT"] -> do
     put $ right st
     walk w h
   ["LEFT"] -> do
     put $ left st
     walk w h
   _ -> return st
   
type Status = ((Int,Int), Dir)

data Dir = N | E | S | W

right :: Status -> Status
right (p, N) = (p, E)
right (p, E) = (p, S)
right (p, S) = (p, W)
right (p, W) = (p, N)

left :: Status -> Status
left = right . right . right

forward :: Int -> Int -> Int -> Status -> Status
forward w h d ((i, j), N) = ((i, min (j+d) h), N)
forward w h d ((i, j), E) = ((min (i+d) w, j), E)
forward w h d ((i, j), S) = ((i, max (j-d) 1), S)
forward w h d ((i, j), W) = ((max (i-d) 1, j), W)

backward :: Int -> Int -> Int -> Status -> Status
backward w h d (p, N) = let (p', _) = forward w h d (p, S) in (p', N)
backward w h d (p, E) = let (p', _) = forward w h d (p, W) in (p', E)
backward w h d (p, S) = let (p', _) = forward w h d (p, N) in (p', S)
backward w h d (p, W) = let (p', _) = forward w h d (p, E) in (p', W)

printPos :: Status -> IO ()
printPos ((i,j), _) = putStrLn . unwords . map show $ [i, j]
