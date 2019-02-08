import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  s <- getLine
  unless (s == "0") $ do
    putStrLn $ solve s
    main

solve :: String -> String
solve s = loop 0 (S.singleton s) (S.singleton s)
  where
    ln = length s
    goal = S.fromList $ map (replicate ln) "rgb"
    loop c st hst
      | S.null st = "NA"
      | not (S.null (S.intersection goal st)) = show c
      | otherwise = let nst = S.foldl' (\x s -> S.union x $ S.fromList $ do
                                           s' <- changeColor ln s
                                           guard $ S.notMember s' hst
                                           return s'
                                       ) S.empty st
                    in loop (c+1) nst (S.union hst nst)

changeColor :: Int -> String -> [String]
changeColor n s = do
  i <- [0 .. n-2]
  let (s1, c1:c2:s2) = splitAt i s
  guard $ c1 /= c2
  case (c1, c2) of
   ('r', 'g') -> return $ s1 ++ "bb" ++ s2
   ('g', 'b') -> return $ s1 ++ "rr" ++ s2
   ('b', 'r') -> return $ s1 ++ "gg" ++ s2
   ('g', 'r') -> return $ s1 ++ "bb" ++ s2
   ('b', 'g') -> return $ s1 ++ "rr" ++ s2
   ('r', 'b') -> return $ s1 ++ "gg" ++ s2
