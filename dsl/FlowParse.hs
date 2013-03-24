module FlowParse (parseFlow) where

import           Control.Applicative ((<$>), (<*>), (<*))
import           Control.Monad.Identity

import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language as T
import qualified Text.Parsec.Token as T

import           UBench

type Parser a = Parsec String () a
type Opr a = Operator String () Identity a

parseFlow :: String -> Either ParseError UTProgram
parseFlow = parse flowProgram "<from string>"
  where
    flowProgram :: Parser UTProgram
    flowProgram = UTProgram <$> (many flowDef <* symbol ";")
                            <*> flowExpr

    flowDef :: Parser UTDef
    flowDef = UTDef <$> identifier
                    <*> identifier
                    <*> identifier

    flowExpr :: Parser UTBench
    flowExpr = e
      where    
        term = parens e
               <|> (UVar <$> identifier)
               <|> (reserved "id" >> return UNoop)
               <|> (reserved "reuse" >> return UReuse)
               <|> (reserved "forget1" >> return UForget1)
               <|> (reserved "swap" >> return USwap)

        secondF a = USwap `USeq` UFirst a `USeq` USwap

        table :: [[Opr UTBench]]
        table = [ [unary "first" UFirst, unary "second" secondF]
                , [binary "***" UPar AssocLeft]
                , [binary ">>>" USeq AssocLeft]
                , [binary "&&&" USplit AssocLeft]
                ]
        binary :: String -> (UTBench -> UTBench -> UTBench) -> Assoc -> Opr UTBench
        binary name f assoc = Infix (reservedOp name >> return f) assoc            
        unary name f        = Prefix (reserved name >> return f)

        e = buildExpressionParser table term

    dslDef = 
      T.emptyDef { T.reservedNames = ["id", "swap", "first", "second", "reuse"
                                     ,"forget1"]
                 , T.reservedOpNames = ["***", ">>>"]
                 }

    tok = T.makeTokenParser dslDef
    
    symbol = T.symbol tok
    integer = T.integer tok
    parens = T.parens tok
    reservedOp = T.reservedOp tok
    reserved = T.reserved tok
    identifier = T.identifier tok
