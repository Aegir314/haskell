module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0) 

symbol :: Parser Char
symbol = oneOf "$!%&|*+-/:<=?>@^_^#"

spaces :: Parser ()
spaces = skipMany1 space

--monadic bind >> does left thing first then right, whole expression fails
--if one of those fails

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

data LispVal = Atom String
             | List [LispVal] 
             | DottedList [LispVal] LispVal --improper list stores list of all elements but the last and then stores the last as another field
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do 
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x --return lifts String x into parser nomad so whole action has type Parser LispVal


--that new operator  tries first parser if fails tries second
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = [first] ++ rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        otherwise -> Atom atom

--many1 matches 1 or more digit
--read converts that string to a number and then we pass that to a Number to
--get ListVal but many1 digit return Parser String so we use liftM to tell
--(Number . read) to just operate on a value inside Monad
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
--try combinator attempts to run specified parser but if it fails it backs up
--to the previous state. Allows using alternative without interfering with the
--other alternative
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> parseString
        <|> parseQuoted
        <|> do 
              char '('
              x<-(try parseList) <|> parseDottedList
              char ')'
              return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x<-parseExpr
                return $ List [Atom "quote", x]

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool false) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _ ) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

