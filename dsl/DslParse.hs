module DslParse (parseDsl, expr) where

import           Control.Monad.Identity

import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language as T
import qualified Text.Parsec.Token as T

import           Dsl
import           Bench

parseDsl :: String -> Either ParseError BenchDsl
parseDsl = parse expr "<from string>"

dslDef = 
  T.emptyDef { T.reservedNames = ["fib", "cache", "<x>", "lock1", "lock2"]
             , T.reservedOpNames = ["|||", ";"]
             }

tok = T.makeTokenParser dslDef

expr = e
  where
    parens = T.parens tok
    reservedOp = T.reservedOp tok
    reserved = T.reserved tok
    
    term = parens e
      <|> (reserved "fib" >> return DslFib)
      <|> (reserved "<x>" >> return DslVar)
      <|> (reserved "cache" >> return DslCache)

    table = [ [unary "lock1" lock1, unary "lock2" lock2]
            , [binary ";"   (|>)  AssocLeft]
            , [binary "|||" (|||) AssocLeft]
            ]
    binary :: String
              -> (BenchDsl -> BenchDsl -> BenchDsl)
              -> Assoc 
              -> Operator String () Identity BenchDsl
    binary name f assoc = Infix (reservedOp name >> return f) assoc            
    unary name f        = Prefix (reserved name >> return f)

    e = buildExpressionParser table term