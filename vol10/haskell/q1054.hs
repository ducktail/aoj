import Control.Applicative
import Control.Monad
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n
    main

solve :: Int -> IO ()
solve n = do
  getLine
  (s, ls) <- getpi
  let ibf = initBuf s
  let idb = M.insert s ls M.empty
  db <- foldM (\m _ -> do
                  (s, ls) <- getpi
                  return $ M.insert s ls m
              ) idb [1..n-1]
  nop <- readLn
  foldM_ (\b _ -> do
             cs <- words <$> getLine
             case cs of
              ["show"] -> do
                putStrLn $ showBuf b
                return b
              ["back"] -> do
                case backBuf b of
                 Just b' -> return b'
                 Nothing -> return b
              ["forward"] -> do
                case forwardBuf b of
                 Just b' -> return b'
                 Nothing -> return b
              ["click", scx, scy] -> do
                let cx = read scx
                    cy = read scy
                    cp = showBuf b
                case find (\(x1, y1, x2, y2, _) -> x1 <= cx && cx <= x2 && y1 <= cy && cy <= y2) (db ! cp) of
                 Just (_, _, _, _, np) -> return $ addBuf b np
                 Nothing -> return b
              _ -> return b
         ) ibf [1..nop]

getpi :: IO (String, [(Int, Int, Int, Int, String)])
getpi = do
  [np, snbt] <- words <$> getLine
  let nbt = read snbt
  lip <- replicateM nbt $ do
    [sx1, sy1, sx2, sy2, nlp] <- words <$> getLine
    let x1 = read sx1
        y1 = read sy1
        x2 = read sx2
        y2 = read sy2
    return (x1, y1, x2, y2, nlp)
  return (np, lip)

type Buf = ([String], [String])

initBuf :: String -> Buf
initBuf s = ([s], [])

addBuf :: Buf -> String -> Buf
addBuf (as, _) s = (s:as, [])

forwardBuf :: Buf -> Maybe Buf
forwardBuf (_,[]) = Nothing
forwardBuf (as, b:bs) = Just (b:as, bs)

backBuf :: Buf -> Maybe Buf
backBuf ([_],_) = Nothing
backBuf (a:as, bs) = Just (as, a:bs)

showBuf :: Buf -> String
showBuf (a:_, _) = a
