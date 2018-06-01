import Control.Applicative ((<$>))
import Control.Monad.State

main :: IO ()
main = solve <$> words <$> getLine >>= print

solve :: [String] -> Int
solve ss = evalState (foldr1 (>>) $ map rpn ss) []

rpn :: String -> State [Int] Int
rpn "+" = do
  (x:y:ys) <- get
  let r = y + x
  put (r:ys)
  return r

rpn "-" = do
  (x:y:ys) <- get
  let r = y - x
  put (r:ys)
  return r

rpn "*" = do
  (x:y:ys) <- get
  let r = y * x
  put (r:ys)
  return r

rpn x = do
  let r = read x
  modify (r:)
  return r
