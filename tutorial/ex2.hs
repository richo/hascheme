module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%^|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "scheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found val: " ++ show val

main :: IO ()
main = do
    line <- getLine
    putStrLn (readExpr line)
    main
