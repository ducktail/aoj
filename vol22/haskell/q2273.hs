import System.IO (hFlush, stdout)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn "?aaaaa"
  hFlush stdout
  solve S.empty

solve :: Set String -> IO ()
solve st = do
  ss <- getLine
  if head ss == 'a' && S.notMember ss st
    then do
      putStrLn $ "?" ++ reverse ss ++ "aa"
      hFlush stdout
      solve (S.insert ss st)
    else do
      putStrLn "!OUT"
      hFlush stdout
