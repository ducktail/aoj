import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Control.Monad.State

main :: IO ()
main = do
  [_, q] <- map read <$> words <$> getLine
  que <- Queue [] <$> map (f . B.words) <$> B.lines <$> B.getContents
  solve q que
  where f [n, t] = (n, readi B.readInt t)
  
solve :: Int -> Queue (ByteString, Int) -> IO ()
solve q que = evalStateT (rrs 0) que
  where rrs :: Int -> StateT (Queue (ByteString, Int)) IO ()
        rrs ct = do
          que <- get
          unless (isEmptyQue que) $ do
            let (Just ((n, t), nque)) = deQue que
            if t > q then do
              put $ enQue (n, t - q) nque
              rrs (ct+q)
            else do
              lift $ putStrLn $ B.unpack n ++ " " ++ show (ct+t)
              put nque
              rrs (ct+t)

data Queue a = Queue [a] [a] deriving Show

enQue :: a -> Queue a -> Queue a
enQue x (Queue xs ys) = Queue (x:xs) ys

deQue :: Queue a -> Maybe (a, Queue a)
deQue (Queue [] []) = Nothing
deQue (Queue xs (y:ys)) = Just (y, Queue xs ys)
deQue (Queue xs []) = deQue (Queue [] (reverse xs))

isEmptyQue :: Queue a -> Bool
isEmptyQue (Queue [] []) = True
isEmptyQue _ = False

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
