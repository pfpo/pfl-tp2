module Parser (parse, Stm(..), Program, Aexp(..), Bexp(..), ABinOp(..), BBinOp(..), RBinOp(..)) where

import Text.Parsec (lower, letter, (<|>), sepEndBy1, char)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator (Infix, Prefix), Assoc (AssocLeft))
import Text.Parsec.Token (GenLanguageDef (identStart, identLetter, reservedNames, reservedOpNames), makeTokenParser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

-- Arithmetic expressions. Can be variables, integers, or binary operations
data Aexp = Var String
            | IntVal Integer
            | ABinary ABinOp Aexp Aexp
            deriving (Show)

-- Arithmetic binary operators. Can be addition, subtraction, or multiplication
data ABinOp = AddOp
              | SubOp
              | Mul
              deriving (Show)

-- Boolean expressions. Can be boolean values, not expressions, binary boolean operations, or binary relational operations
data Bexp = BoolVal Bool
            | Not Bexp
            | BBinary BBinOp Bexp Bexp
            | RBinary RBinOp Aexp Aexp
            deriving (Show)

-- Boolean binary operators. Can be and or equal
data BBinOp = AndOp
              | BEqual
              deriving (Show)

-- Relational binary operators. Can be equal or less or equal
data RBinOp = Equal
              | LessEq
              deriving (Show)

-- Statements. Can be a sequence of statements, an assignment, an if statement, or a while statement
data Stm = Seq [Stm]
           | Assign String Aexp
           | If Bexp Stm Stm
           | While Bexp Stm
           deriving (Show)

-- Program. Just a statement (which can be a sequence of statements)
type Program = Stm

-- Parsec language definition. Defines variable name syntax, reserved words and reserved operators
language =
    emptyDef {
        identStart = lower,
        identLetter = letter,
        reservedNames = ["if", "then", "else", "while", "do", "not", "and", "True", "False"],
        reservedOpNames = ["+", "-", "*", ":=", "<=", "=", "and", "not"]
    }

-- Parsec lexer. Defines how to parse whitespace, identifiers, reserved words
-- reserved operators, parentheses, integers and semicolons
lexer = makeTokenParser language

identifier = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
integer = T.integer lexer
semi = T.semi lexer
whiteSpace = T.whiteSpace lexer

-- Initial parser, which parses whitespace and then a statement
initParser :: Parser Stm
initParser = whiteSpace >> statement

-- Statement parser, which parses a statement surrounded by parentheses or a sequence of statements
statement :: Parser Stm
statement = parens statement <|> sequenceOfStm

-- Sequence of statements parser, which parses statements separated by semicolons
sequenceOfStm :: Parser Stm
sequenceOfStm = do
    stms <- sepEndBy1 statement' semi
    return $ case stms of
        [stm] -> stm
        _     -> Seq stms

-- Statement' parser, which parses an if statement, a while statement or an assignment
statement' :: Parser Stm
statement' = ifParser
            <|> whileParser
            <|> assignParser

-- If statement parser, which parses statements of the form if (condition) then (body) else (body)
ifParser :: Parser Stm
ifParser = If <$> (reserved "if" *> bExpr) <*> (reserved "then" *> (parens statement <|> statement' <* semi)) <*> (reserved "else" *> (parens statement <|> statement'))

-- While statement parser, which parses statements of the form while (condition) do (body)
whileParser :: Parser Stm
whileParser = While <$> (reserved "while" *> bExpr) <*> (reserved "do" *> (parens statement <|> statement'))

-- Assignment parser, which parses statements of the form variable := expression
assignParser :: Parser Stm
assignParser = Assign <$> identifier <*> (reservedOp ":=" *> aExpr)

-- Arithmetic expression parser, which parses an arithmetic expression based on the defined operators and their precedences and possible terms
aExpr :: Parser Aexp
aExpr = buildExpressionParser aExprOperators aExprTerm

-- Arithmetic operators table, which is ordered by descending precedence.
aExprOperators = [ [Infix (ABinary Mul <$ reservedOp "*") AssocLeft],
                   [Infix (ABinary AddOp <$ reservedOp "+") AssocLeft, Infix (ABinary SubOp <$ reservedOp "-") AssocLeft]]

-- Arithmetic expression term parser, which parses an arithmetic expression surrounded by parentheses, a variable or an integer
aExprTerm = parens aExpr <|> varParser <|> intParser

-- Variable parser, which parses an identifier
varParser :: Parser Aexp
varParser = Var <$> identifier

-- Integer parser, which parses an integer
intParser :: Parser Aexp
intParser = IntVal <$> integer

-- Boolean expression parser, which parses a boolean expression based on the defined operators and their precedences and possible terms
bExpr :: Parser Bexp
bExpr = buildExpressionParser bExprOperators bExprTerm

-- Boolean operators table, which is ordered by descending precedence.
bExprOperators = [ [Prefix (Not <$ reservedOp "not")],
                   [Infix (BBinary BEqual <$ reservedOp "=") AssocLeft],
                   [Infix (BBinary AndOp <$ reservedOp "and") AssocLeft]]

-- Boolean expression term parser, which parses a boolean expression surrounded by parentheses, a boolean value or a relational expression
bExprTerm = parens bExpr <|> boolParser <|> rExpr

-- Boolean value parser, which parses a boolean value
boolParser :: Parser Bexp
boolParser = BoolVal <$> ((True <$ reserved "True") <|> (False <$ reserved "False"))

-- Relational expression parser, which parses a relational expression
rExpr :: Parser Bexp
rExpr = do
    left <- aExpr
    op <- rOpParser
    RBinary op left <$> aExpr

-- Relational operator parser, which parses a relational operator (equal / less or equal)
rOpParser :: Parser RBinOp
rOpParser = (LessEq <$ reservedOp "<=") <|> (Equal <$ reservedOp "==")

-- Top level parser, which parses a string into a program (statement)
parse :: String -> Program
parse input = 
    case P.parse initParser "" input of
        Left err -> error $ show err
        Right x -> x
