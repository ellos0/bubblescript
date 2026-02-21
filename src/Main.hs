module Main
where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.IO
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

whitespaceChar :: Parser Char
whitespaceChar = (space <|> tab <|> newline)

whitespaces :: Parser ()
whitespaces = skipMany1 (whitespaceChar)

maybeWhitespace :: Parser ()
maybeWhitespace = skipMany (whitespaceChar)

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         ":t" -> Bool True
                         ":f" -> Bool False
                         _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr whitespaces

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> do
                _ <- char '('
                x <- try parseList
                _ <- char ')'
                return x

instance Show LispVal where show = showVal

rep :: String -> String
rep input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> showC val

compileFile :: FilePath -> IO ()
compileFile f = do
  inputHandle <- openFile f ReadMode
  contents <- hGetContents inputHandle
  
  case parse parseExpr "lisp" contents of
    Right val -> (print (showC val))
    Left err -> (print ("Error: " ++ show err))

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = ":t"
showVal (Bool False) = ":f"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"

showC :: LispVal -> String
showC (Atom x) = x
showC (Number x) = show x
showC (String x) = "\"" ++ x ++ "\""
showC (Bool x) = if x then "1" else "0"
showC (List []) = ""
showC (List (op:args)) = case (op) of
  (Atom "+") -> concat [showC (args !! 0), "+", showC (args !! 1)]
  (Atom "-") -> concat [showC (args !! 0), "-", showC (args !! 1)]
  (Atom "*") -> concat [showC (args !! 0), "*", showC (args !! 1)]
  (Atom "/") -> concat [showC (args !! 0), "/", showC (args !! 1)]
  (Atom "print") -> concat ["printf(", showC (args !! 0), ");\n"]
  (Atom "defun") -> concat [showC (args !! 1), " ", showC (args !! 0), "(){\n", showC (args !! 2), "}"]
  (Atom "type") -> showC (args !! 0)
  (List _) -> concat (map showC (op:args))
  _ -> ""

main :: IO ()
main = do
  args <- getArgs
  compileFile (args !! 0)
