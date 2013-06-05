module Main where
import Text.ParserCombinators.Parsec
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%^|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "scheme" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found val: " ++ show val

main :: IO ()
main = do
    line <- getLine
    putStrLn (readExpr line)
    main
