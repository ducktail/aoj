import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= mapM_ putStrLn
    main
  where
    f = g <$> getLine
    g s = let (as, bs) = break (/= '.') s
              l = length as
          in (l, if l == 0 then s else replicate (l - 1) ' ' ++ '+':bs)

solve :: [(Int, String)] -> [String]
solve ss = f [] ss
  where
    f st [] = map snd . reverse $ st
    f st ((i,x):xs) = let (as, bs) = span (\(j, _) -> i < j) st
                      in if null as
                         then f ((i,x):st) xs
                         else f ((i,x):(map (\(u, v) -> (u, insPipe i v)) as ++ bs)) xs

insPipe :: Int -> String -> String
insPipe i s = let (as, (_:bs)) = splitAt (i - 1) s
              in as ++ '|':bs
