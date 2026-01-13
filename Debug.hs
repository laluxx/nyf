module Debug where
import qualified Data.Text as T
import Text.Megaparsec (parse, errorBundlePretty)
import Parser (file, expr, stmt, defStmt)

--- Quick test function for the REPL
testParse :: String -> IO ()
testParse input = do
  let text = T.pack input
  case parse file "<test>" text of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> print ast

--- Test just an expression
testExpr :: String -> IO ()
testExpr input = do
  let text = T.pack input
  case parse expr "<test>" text of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> print ast

--- Test just a statement
testStmt :: String -> IO ()
testStmt input = do
  let text = T.pack input
  case parse stmt "<test>" text of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> print ast

--- Test defStmt directly
testDefStmt :: String -> IO ()
testDefStmt input = do
  let text = T.pack input
  case parse defStmt "<test>" text of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> print ast
