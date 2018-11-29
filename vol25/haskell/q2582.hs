import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> f >>= print
    main
  where
    f = words <$> getLine

solve :: [String] -> Int
solve = subtract 1 . length . f [DD] DD 
  where
    f ls st [] = ls
    f ls DD ("lu" : cs) = f ls UD cs
    f ls DD ("ru" : cs) = f ls DU cs
    f ls UD ("ld" : cs) = f (if head ls == DD then ls else DD:ls) DD cs
    f ls UD ("ru" : cs) = f (if head ls == UU then ls else UU:ls) UU cs
    f ls DU ("lu" : cs) = f (if head ls == UU then ls else UU:ls) UU cs
    f ls DU ("rd" : cs) = f (if head ls == DD then ls else DD:ls) DD cs
    f ls UU ("rd" : cs) = f ls UD cs
    f ls UU ("ld" : cs) = f ls DU cs
    
data State = DD | DU | UD | UU deriving (Show, Eq)
