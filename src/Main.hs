module Main
where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.IO

import Codegen
import Struct

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

parseSingleComment :: Parser ()
parseSingleComment = do
    _ <- string "//"
    _ <- manyTill anyChar newline
    return ()

parseMultiComment :: Parser ()
parseMultiComment = do
  _ <- string "/*"
  _ <- manyTill anyChar (try (string "*/"))
  return ()

parseSkipComments :: Parser ()
parseSkipComments = do
  _ <- skipMany (parseSingleComment <|> parseMultiComment)
  return ()

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
parseList = liftM List $ sepEndBy parseExpr maybeWhitespace

parseFullList :: Parser LispVal
parseFullList = do
  _ <- char '('
  _ <- maybeWhitespace
  x <- parseList
  _ <- maybeWhitespace
  _ <- char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseFullList

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

compileCode :: String -> IO ()
compileCode code = do
  case parse parseExpr "lisp" code of
    Right val -> (print (showC val))
    Left err -> (hPutStrLn stderr ("Error: " ++ show err))

main :: IO ()
main = do
  code <- getContents
  compileCode code
