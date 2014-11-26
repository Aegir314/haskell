module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

main :: IO ()
main = do
         args <- getArgs
         evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
         putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "$!%&|*+-/:<=?>@^_^#"

spaces :: Parser ()
spaces = skipMany1 space

--monadic bind >> does left thing first then right, whole expression fails
--if one of those fails

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _ ) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal 
apply func args = maybe ( throwError $ NotFunction "Unrecognized primitive function args" func) ($args) (lookup func primitives)
--lookup searcher by keyi, primitives is list of pairs of string and functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =  [( "+", numericBinop (+)),
               ( "-", numericBinop (-)),
               ( "*", numericBinop (*)),
               ( "/", numericBinop div),
               ( "mod", numericBinop mod),
               ( "quotient", numericBinop quot),
               ( "remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
--reads returns list of pairs of (in our case:)(parsed value, original value)
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String

showError (UnboundVar message varname) = message ++ ": " ++ varname

showError (BadSpecialForm message form) = message ++ ": " ++ show form

showError (NotFunction message func) = message ++ ": " ++ show func

showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args: found values " ++ 
                                     unwordsList found 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found

showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

--error goes to left so we leave it undefined 
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


