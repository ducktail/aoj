import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  s <- getLine
  unless (s == ".") $ do
    print $ solve s
    main

solve :: String -> Int
solve s = length . filter f $ ls
  where
    f :: (Char, Char, Char) -> Bool
    f x = '2' == either (const ' ') id (parse formula "" (tr s x))
    ls = [(p,q,r)|p <- "012", q <- "012", r <- "012"]

minus :: Char -> Char
minus '0' = '2'
minus '1' = '1'
minus _ = '0'

mul :: Char -> Char -> Char
mul '2' '2' = '2'
mul '0' _ = '0'
mul _ '0' = '0'
mul _ _ = '1'

add :: Char -> Char -> Char
add '0' '0' = '0'
add '2' _ = '2'
add _ '2' = '2'
add _ _ = '1'

addmul :: Char -> Char -> Char -> Char
addmul x '*' y = mul x y
addmul x _ y = add x y

formula :: Parser Char
formula = num <|> ht <|> skwa

num :: Parser Char
num = char '0' <|> char '1' <|> char '2'

ht :: Parser Char
ht = char '-' *> (minus <$> formula)

skwa :: Parser Char
skwa = char '(' *> (addmul <$> formula <*> (char '*' <|> char '+') <*> formula) <* char ')'

tr :: String -> (Char, Char, Char) -> String
tr expr (x,y,z) = map g expr
  where
    g 'P' = x
    g 'Q' = y
    g 'R' = z
    g w = w

getl :: (String -> a) -> IO a
getl f = f <$> getLine
