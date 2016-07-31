wimport Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Control.Monad.State

main :: IO ()
main = do
  lines <$> getContents >>= solve

solve :: [String] -> IO ()
solve xs = runStateT (mapM_ calc xs) ("?",0) >> return ()

calc :: String -> StateT (String,Int) IO ()
calc x
  | x == "+" = modify (\(s,y) -> ("+",y))
  | x == "-" = modify (\(s,y) -> ("-",y))
  | x == "*" = modify (\(s,y) -> ("*",y))
  | x == "/" = modify (\(s,y) -> ("/",y))
  | x == "=" = do
      (_,y) <- get
      liftIO $ print y
      put ("?",0)
  | otherwise = do
      (s,y) <- get
      case s of
       "+" -> put ("?", y + toInt x)
       "-" -> put ("?", y - toInt x)
       "*" -> put ("?", y * toInt x)
       "/" -> put ("?", y `div` (toInt x))
       _ -> put ("?", toInt x)

toInt :: String -> Int
toInt = read
