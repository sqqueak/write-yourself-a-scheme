module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String                      -- String naming the atom
             | List [LispVal]                   -- List of other LispVals
             | DottedList [LispVal] LispVal     -- List, then tail of the list
             | Number Integer                   -- Haskell int
             | String String                    -- Haskell string
             | Bool Bool                        -- Haskell bool

main :: IO ()
main = do
    (expr:_) <- getArgs                     -- appends input to the head of a list
    putStrLn (readExpr expr)                -- prints result of calling match function it

symbol :: Parser Char                       -- is of type Parser which recognizes Chars
symbol = oneOf "!#$%&|*+-/:<=>?@^_"         -- recognizes when a char in the str is passed into it

spaces :: Parser ()                         -- parser that can skip any amount of whitespace
spaces = skipMany1 space                    -- pass the action "space" to the parser action "skipMany1"

readExpr :: String -> String                -- function which takes in a String and returns a String
readExpr input = case parse parseExpr "path" input of
    Left err -> "No match:" ++ show err             -- "parse symbol "lisp" input" == parse p filepath input
    Right _ -> "Found value"                      -- call Parsec's "parse" function, which takes in a parser p
                                                    -- (our symbol object), a filepath (idk), and an input string;
                                                    -- this returns an Either object, so pattern match on that
-- readExpr input = case parse (spaces >> symbol) "path" input of  -- >> does Parser bind, attempts to match input with first then second parser

parseString :: Parser LispVal
parseString = do
    char '"'                                -- parse out opening "
    x <- many (noneOf "\"")                 -- applies the parser (noneOf "\"") zero or more times, which input as long as it's not "
    char '"'                                -- parse out closing "
    return $ String x                       -- equivalent to return (String x)

parseAtom :: Parser LispVal
parseAtom = do                              -- seems like a parser for variable names (first ch = letter/symbol) etc
    first <- letter <|> symbol              -- <|> is choice operator, it will try first then second (else it throws?)
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest                   -- parsed representation of characters (variable)
    return $ case atom of                   -- determining which LispVal to return
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    (many1 digit) >>= \x -> return ((Number . read) $ x)
    -- ps <- many1 digit
    -- return ((Number . read) ps)
-- i literally have no idea how this works
-- parseNumber = liftM (Number . read) $ many1 digit   -- (Number . read) creates a function that applies read and passes it to Number
                                                    -- many1 digit matches one or more digits and returns Parser String
                                                    -- liftM (idk) is applied to (Number . read) which is applied to many1 digit 's result

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces     -- "sepBy parseExpr spaces" takes in string parseExpr and returns a list of elts in 
                                                    -- parseExpr separacted by the parser spaces; the value is lifted out of the monad (???)
                                                    -- and made into a List

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

-------------

