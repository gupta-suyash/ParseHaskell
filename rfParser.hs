-- Parser based on RFGrammar

import System.IO

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char

data Expr 	= Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr 
		deriving Show
data Unop 	= Not deriving Show
data Duop 	= And | Or | Geq | Sub | Dot | Iff deriving Show
data Stmt 	= Nop 
		| String := Expr 
		| If Expr Stmt Stmt 
		| Assert (Expr)
		| Transaction Stmt
		| Foreach Stmt
		| Seq [Stmt]
		deriving Show

data Param 	= PList [Expr] deriving Show
data Method 	= Mtd String Param Stmt deriving Show 
--data Goal 	= MList [Method] deriving Show


def = emptyDef{ commentStart = "{-"
	, commentEnd = "-}"
	, identStart = letter
	, identLetter = alphaNum
	, opStart = oneOf "!&>=:|-."
	, opLetter = oneOf "!&>=:|-."
	, reservedOpNames = ["!", "&", ">=", "=", "-", ":=", "|", ".", "(", ")"]
	, reservedNames = ["true", "false", "nop",
			"if", "then", "else", "end",
			"assert", "do", "foreach", "def",  
			"transaction"]
	}


TokenParser{ parens = m_parens
	, identifier = m_identifier
	, reservedOp = m_reservedOp
	, reserved = m_reserved
	, semiSep1 = m_semiSep1
	, commaSep1 = m_commaSep1
	, whiteSpace = m_whiteSpace } = makeTokenParser def


exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "!" >> return (Uno Not))]
	, [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
	, [Infix (m_reservedOp "|" >> return (Duo Or)) AssocLeft]
	, [Infix (m_reservedOp ">=" >> return (Duo Geq)) AssocLeft]
	, [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
	, [Infix (m_reservedOp "." >> return (Duo Dot)) AssocLeft]
	, [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
	]
term = m_parens exprparser
	<|> fmap Var m_identifier
	<|> (m_reserved "true" >> return (Con True))
	<|> (m_reserved "false" >> return (Con False))


-- Accepts comma separated arguments
paramparser :: Parser Param
paramparser = m_whiteSpace >> argparser
	where
	  argparser :: Parser Param
	  argparser = fmap PList (m_commaSep1 exprparser)


-- Accepts a method, but while specifing parameters there is a need of space between
-- brackets "(" and ")" and arguments.
methodparser :: Parser Method
methodparser = m_whiteSpace >> functparser <* eof
	where
	  functparser :: Parser Method
	  functparser = mthd
	  mthd = do 	{ m_reserved "def"
			; v <- m_identifier
			; m_reserved "("
			--; m_whiteSpace
			; p <- paramparser
			; m_reservedOp ")"
			; s <- statementparser
			; m_reserved "end"
			; return (Mtd v p s)
			}


-- Every statement is separated by a semi-colon. We have used semiSep1.
statementparser :: Parser Stmt
statementparser = m_whiteSpace >> stmtparser -- <* eof
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
play inp = case parse methodparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }


--play inp = case parse mainparser "" inp of
--             { Left err -> print err
--             ; Right ans -> print ans
--             }

main :: IO ()
main = do
	putStrLn "Starting Parser"
	putStrLn "Enter filename: "
	fname <- getLine
	fhandle <- openFile fname ReadMode  
    	contents <- hGetContents fhandle  
	putStrLn "Input to parse: "
    	putStr contents  
	play contents
	hClose fhandle
	return ()
