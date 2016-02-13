module SQLParser.PGSqlParser
(
parseSql
)where

import System.Environment

import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Database.HsSqlPpp.TypeChecker
import Database.HsSqlPpp.Catalog
import Database.HsSqlPpp.Types
import Database.HsSqlPpp.Annotation
import Database.HsSqlPpp.Ast
import Database.HsSqlPpp.Parser


showNoAnns :: Show a => a -> String
showNoAnns = p stripA
  where
    stripA :: Exp -> Exp
    stripA = transformBi $ \x ->
               case x of
                 (Paren (RecConstr (UnQual (Ident "Annotation")) _)) ->
                          Con $ UnQual $ Ident "Ann"
                 x1 -> x1
    p f s =
        case parseExp (show s) of
          ParseOk ast -> prettyPrint (f ast)
          x -> error $ show x


parseSql inp =  case parseStatements "" inp of
             { Left err -> show err 
             ; Right e ->  show (showNoAnns e) 
             }

