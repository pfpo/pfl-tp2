import Text.Parsec (lower, letter, (<|>), sepEndBy1, char)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator (Infix, Prefix), Assoc (AssocLeft))
import Text.Parsec.Token (GenLanguageDef (identStart, identLetter, reservedNames, reservedOpNames), makeTokenParser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

data Aexp = Var String
            | IntVal Integer
            | ABinary ABinOp Aexp Aexp
            deriving (Show)

data ABinOp = Add
              | Sub
              | Mul
              | Div
              deriving (Show)

data Bexp = BoolVal Bool
            | Not Bexp
            | BBinary BBinOp Bexp Bexp
            | RBinary RBinOp Aexp Aexp
            deriving (Show)

data BBinOp = And
              | BEqual
              deriving (Show)

data RBinOp = Equal
              | LessEq
              deriving (Show)

data Stm = Seq [Stm]
            | Assign String Aexp
            | If Bexp Stm Stm
            | While Bexp Stm
            | End Stm
            deriving (Show)

language =
    emptyDef {
        identStart = lower,
        identLetter = letter,
        reservedNames = ["if", "then", "else", "while", "do", "not", "and", "True", "False"],
        reservedOpNames = ["+", "-", "*", ":=", "<=", "=", "and", "not"]
    }

lexer = makeTokenParser language

identifier = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
integer = T.integer lexer
semi = T.semi lexer
whiteSpace = T.whiteSpace lexer

initParser :: Parser Stm
initParser = whiteSpace >> statement

statement :: Parser Stm
statement = parens statement <|> sequenceOfStm

sequenceOfStm :: Parser Stm
sequenceOfStm = do
    stms <- sepEndBy1 statement' semi
    return $ case stms of
        [stm] -> stm
        _      -> Seq stms

statement' :: Parser Stm
statement' = ifParser
            <|> whileParser
            <|> assignParser

ifParser :: Parser Stm
ifParser = If <$> (reserved "if" *> bExpr) <*> (reserved "then" *> statement) <*> (reserved "else" *> statement)

whileParser :: Parser Stm
whileParser = While <$> (reserved "while" *> bExpr) <*> (reserved "do" *> statement)

assignParser :: Parser Stm
assignParser = Assign <$> identifier <*> (reservedOp ":=" *> aExpr)

aExpr :: Parser Aexp
aExpr = buildExpressionParser aExprOperators aExprTerm

aExprOperators = [ [Infix (ABinary Mul <$ reservedOp "*") AssocLeft],
                   [Infix (ABinary Add <$ reservedOp "+") AssocLeft, Infix (ABinary Sub <$ reservedOp "-") AssocLeft]]

aExprTerm = parens aExpr <|> varParser <|> intParser

varParser :: Parser Aexp
varParser = Var <$> identifier

intParser :: Parser Aexp
intParser = IntVal <$> integer

bExpr :: Parser Bexp
bExpr = buildExpressionParser bExprOperators bExprTerm

bExprOperators = [ [Prefix (Not <$ reservedOp "not")],
                   [Infix (BBinary BEqual <$ reservedOp "=") AssocLeft],
                   [Infix (BBinary And <$ reservedOp "and") AssocLeft]
                ]

bExprTerm = parens bExpr <|> boolParser <|> rExpr

boolParser :: Parser Bexp
boolParser = BoolVal <$> ((True <$ reserved "True") <|> (False <$ reserved "False"))

rExpr :: Parser Bexp
rExpr = do
    left <- aExpr
    op <- rOpParser
    RBinary op left <$> aExpr

rOpParser :: Parser RBinOp
rOpParser = (Equal <$ reservedOp "<=") <|> (LessEq <$ reservedOp "==")

parse :: String -> Stm
parse input = 
    case P.parse initParser "" input of
        Left err -> error $ show err
        Right x -> x
