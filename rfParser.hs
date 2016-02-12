-- Parser based on RFGrammar

import System.IO

import Control.Applicative((<*))
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char

data Expr 	= Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr 
		| Dot Expr Expr deriving Show
data Unop 	= Not deriving Show
data Duop 	= And | Or | Geq | Sub | Iff deriving Show
data Stmt 	= Nop 
		| String := Expr 
		| IfEl Expr Stmt Stmt 
		| If Expr Stmt
		| Assert (Expr)
		| Transaction Stmt
		| Foreach Stmt
		| Throw Expr
		| Seq [Stmt]
		deriving Show

data Param 	= PList [Expr] deriving Show
data Method 	= Mtd String Param Stmt 
		| MList [Method] deriving Show 


def = emptyDef{ commentStart = "{-"
	, commentEnd = "-}"
	, identStart = letter
	, identLetter = alphaNum
	, opStart = oneOf "!&>=:|-."
	, opLetter = oneOf "!&>=:|-."
	, reservedOpNames = ["!", "&", ">=", "=", "-", ":=", "|", "."]
	, reservedNames = ["true", "false", "nop", "throw", 
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


-- Accepting different form of whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

leftparan :: Parser Char
leftparan = char '('

rightparan :: Parser Char
rightparan = char ')'

-- Accepts expressions.
exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "!" >> return (Uno Not))]
	, [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
	, [Infix (m_reservedOp "|" >> return (Duo Or)) AssocLeft]
	, [Infix (m_reservedOp ">=" >> return (Duo Geq)) AssocLeft]
	, [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
	, [Infix (m_reservedOp "." >> return (Dot)) AssocLeft]
	, [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
	]
term = m_parens exprparser
	<|> fmap Var m_identifier
	<|> (m_reserved "true" >> return (Con True))
	<|> (m_reserved "false" >> return (Con False))


-- Accepts comma separated arguments
paramparser :: Parser Param
paramparser = whitespace >> argparser
	where
	  argparser :: Parser Param
	  argparser = fmap PList (m_commaSep1 exprparser)


-- Accepts one or more methods.
methodparser :: Parser Method
methodparser = whitespace >> functparser <* eof
	where
	  functparser :: Parser Method
	  functparser = fmap MList (many1 mthd)
	  mthd = do 	{ m_reserved "def"
			; v <- m_identifier
			; leftparan
			; p <- paramparser
			; rightparan
			; s <- statementparser
			; whitespace
			; m_reserved "end"
			; return (Mtd v p s)
			}


-- Every statement is separated by a semi-colon. We have used semiSep1.
-- The last statement should not have a semi-colon.
statementparser :: Parser Stmt
statementparser = whitespace >> stmtparser 
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = try nopst <|> try assignst <|> try ifelsest <|> try ifst <|> try transtst <|> try assertst <|> try foreachst <|> try throwst

nopst = do 	{ m_reserved "nop" 
		; return Nop
		}

assignst = do 	{ v <- m_identifier
		; m_reservedOp ":="
		; e <- exprparser
		; return (v := e)
		}
ifelsest = do 	{ m_reserved "if"
		; b <- exprparser
		; whitespace
		; m_reserved "then"
		; p <- statementparser
		; whitespace
		; m_reserved "else"
		; q <- statementparser
		; whitespace
		; m_reserved "end"
		; return (IfEl b p q)
		}
ifst =  do 	{ m_reserved "if"
		; b <- exprparser
		; whitespace
		; m_reserved "then"
		; p <- statementparser
		; whitespace
		; m_reserved "end"
		; return (If b p)
		}
assertst = do 	{ m_reserved "assert"
		; b <- exprparser
		; return (Assert b)
		}
transtst = do	{ m_reserved "transaction"
		; whitespace
		; m_reserved "do"
		; p <- statementparser
		; whitespace
		; m_reserved "end"
		; return (Transaction p)
		}
foreachst = do	{ m_reserved "foreach"
		; exprparser
		; m_reservedOp "|"
		; exprparser
		; m_reservedOp "|"
		; m_reserved "do" 
		; st <- statementparser
		; whitespace
		; m_reserved "end"
		; return (Foreach st)
		} 
throwst = do	{ m_reserved "throw"
		; e <- exprparser
		; return (Throw e)
		}

play :: String -> IO ()
play inp = case parse methodparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }


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
