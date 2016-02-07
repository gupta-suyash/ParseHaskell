-- Parser based on RFGrammar

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
		deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop 
	| String := Expr 
	| If Expr Stmt Stmt 
	| Assert (Expr)
	| Transaction Stmt
	| Foreach Stmt
	| Seq [Stmt]
		deriving Show

data Param 	= PList [Param]
		| String
data Method 	= Method String Param Stmt 
data Goal 	= MList [Method]


def = emptyDef{ commentStart = "{-"
	, commentEnd = "-}"
	, identStart = letter
	, identLetter = alphaNum
	, opStart = oneOf "~&=:"
	, opLetter = oneOf "~&=:"
	, reservedOpNames = ["~", "&", "=", ":=", "|"]
	, reservedNames = ["true", "false", "nop",
			"if", "then", "else", "end",
			"assert", "do", "foreach",  
			"transaction"]
	}


TokenParser{ parens = m_parens
	, identifier = m_identifier
	, reservedOp = m_reservedOp
	, reserved = m_reserved
	, semiSep1 = m_semiSep1
	, whiteSpace = m_whiteSpace } = makeTokenParser def


exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
	, [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
	, [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
	]
term = m_parens exprparser
	<|> fmap Var m_identifier
	<|> (m_reserved "true" >> return (Con True))
	<|> (m_reserved "false" >> return (Con False))

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
		<|> do { v <- m_identifier
		       ; m_reservedOp ":="
		       ; e <- exprparser
		       ; return (v := e)
		       }
		<|> do { m_reserved "if"
		       ; b <- exprparser
		       ; m_reserved "then"
		       ; p <- stmtparser
		       ; m_reserved "else"
		       ; q <- stmtparser
		       ; m_reserved "end"
		       ; return (If b p q)
		       }
		<|> do 	{ m_reserved "assert"
			; b <- exprparser
			; return (Assert b)
			}
		<|> do	{ m_reserved "transaction"
			; m_reserved "do"
			; p <- stmtparser
			; m_reserved "end"
			; return (Transaction p)
			}
		<|> do	{ m_reserved "foreach"
			; exprparser
			; m_reservedOp "|"
			; exprparser
			; m_reservedOp "|"
			; m_reserved "do" 
			; st <- stmtparser
			; m_reserved "end"
			; return (Foreach st)
			} 

play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

main :: IO ()
main = do
	putStrLn "Hello World"
	name <- getLine 
	play name
	return ()
