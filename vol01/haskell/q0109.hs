import Control.Applicative
import Control.Monad
import Data.List

import Text.Parsec (parse, many1, digit, char)
import Text.Parsec.String

-- import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- readLn
  map solve <$> replicateM n (init <$> getLine) >>= mapM_ print

solve :: String -> Int
solve = either (const 0) id . parse expr ""

num :: Parser Int
num = read <$> many1 digit

fact :: Parser Int
fact = do
  sgn <- negate <$ char '-' <|> id <$ char '+' <|> return id
  sgn <$> (num <|> char '(' *> expr <* char ')')

term :: Parser Int
term = do
  f1 <- fact
  ops <- many $ do
    op <- flip (*) <$ char '*' <|> flip quot <$ char '/'
    op <$> fact
  return $ foldl' (flip ($)) f1 ops

expr :: Parser Int
expr = do
  t1 <- term
  ops <- many $ do
    op <- flip (+) <$ char '+' <|> flip (-) <$ char '-'
    op <$> term
  return $ foldl' (flip ($)) t1 ops
  
-- tests :: [Test]
-- tests = map f [
--   ("num1", parse num "" "00234", Right 234),
--   ("term1", parse term "" "23", Right 23),
--   ("term2", parse term "" "2*3", Right 6),
--   ("term3", parse term "" "7/3", Right 2),
--   ("term4", parse term "" "23*43/124", Right 7),
--   ("term5", parse term "" "352345/89/38", Right 104),
--   ("expr1", parse expr "" "345", Right 345),
--   ("expr2", parse expr "" "4*3*5/12", Right 5),
--   ("expr3", parse expr "" "2+3-1", Right 4),
--   ("expr4", parse expr "" "12-3+5", Right 14),
--   ("expr5", parse expr "" "2*3*3-12/3*2+5*8/2", Right 30),
--   ("fact1", parse fact "" "(3+5*2)", Right 13),
--   ("term6", parse term "" "(3+5*2)*(8-25/5)", Right 39),
--   ("expr6", parse expr "" "4-2*3", Right (-2)),
--   ("expr7", parse expr "" "4*(8+4+3)", Right 60),
--   ("expr8", parse expr "" "-3*-5+-60/5", Right 3),
--   ("fact2", parse fact "" "34", Right 34),
--   ("fact3", parse fact "" "+45", Right 45),
--   ("fact4", parse fact "" "-567", Right (-567)),
--   ("fact5", parse expr "" "(-99)/4", Right (-24)),
--   ("fact6", parse expr "" "-5++8*(23+4-2*3)-(-(35-2)*3)/4", Right 187)
--   ]
--   where 
--     f (ms, rv, ev) = ms ~: rv ~?= ev
