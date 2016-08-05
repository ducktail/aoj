import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  solve <$> getc (map words) >>= mapM_ print

solve :: [[String]] -> [BTree]
solve = map f
  where
    f :: [String] -> BTree
    f [c, t1, t2]
      | c == "i" = isect l r
      | otherwise = unn l r
      where
        l = either (const Leaf) id $ parse expr "" t1
        r = either (const Leaf) id $ parse expr "" t2
        
getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents

expr :: Parser BTree
expr = do
  char '('
  l <- expr <|> return Leaf
  char ','
  r <- expr <|> return Leaf
  char ')'
  return $ Node l r

data BTree = Leaf | Node BTree BTree

instance Show BTree where
  show Leaf = ""
  show (Node l r) = "(" ++ show l ++ ","++ show r  ++ ")"

isect :: BTree -> BTree -> BTree
isect Leaf _ = Leaf
isect _ Leaf = Leaf
isect (Node l1 r1) (Node l2 r2) = Node (isect l1 l2) (isect r1 r2)

unn :: BTree -> BTree -> BTree
unn Leaf r = r
unn l Leaf = l
unn (Node l1 r1) (Node l2 r2) = Node (unn l1 l2) (unn r1 r2)
