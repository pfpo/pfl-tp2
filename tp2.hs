import Data.Char
import qualified Text.Parsec as P
import Text.Parsec (try, (<|>), (<?>), string, many, many1, space, spaces, letter, char, digit)
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellDef)

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

---------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------
-- Stack data type. Holds a list of StkVal which can be ints or bools
data StkVal = IntVal Integer | BoolVal Bool
data Stack = Stk [StkVal]

instance Eq StkVal where
  (IntVal i1) == (IntVal i2) = i1 == i2
  (BoolVal b1) == (BoolVal b2) = b1 == b2
  _ == _ = False

push :: StkVal -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> StkVal
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

emptyStack :: Stack
emptyStack = Stk []

stkVal2Str :: StkVal -> String
stkVal2Str (IntVal i) = show i
stkVal2Str (BoolVal b) = show b

-- State data type. Holds a list of (String, StkVal) pairs
type State = [(String, StkVal)]

emptyState :: State
emptyState = []

-- Store a value in the state. If the variable already exists, replace it. If it doesn't, add it. Should be ordered alphabetically.
store :: String -> StkVal -> State -> State
store str val [] = [(str, val)]
store str val ((str', val'):xs)
    | str == str' = (str, val):xs
    | str < str' = (str, val):(str', val'):xs
    | otherwise = (str', val'): store str val xs

-- Fetch a value from the state. If the variable doesn't exist, return an error.
fetch :: String -> State -> StkVal
fetch str [] = error "Stack.fetch: variable not found"
fetch str ((str', val'):xs)
    | str == str' = val'
    | otherwise = fetch str xs

---------------------------------------------------------------------

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


createEmptyStack :: Stack
createEmptyStack = emptyStack

stack2Str :: Stack -> String
stack2Str (Stk []) = ""
stack2Str (Stk [x]) = stkVal2Str x
stack2Str (Stk (x:xs)) = stkVal2Str x ++ "," ++ stack2Str (Stk xs)

createEmptyState :: State
createEmptyState = emptyState

state2Str :: State -> String
state2Str [] = ""
state2Str [(str, val)] = str ++ "=" ++ stkVal2Str val
state2Str ((str, val):xs) = str ++ "=" ++ stkVal2Str val ++ "," ++ state2Str xs

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push i):code, stack, state) = run (code, push (IntVal i) stack, state)
run (Add:code, stack, state) = run (code, push (IntVal (i1 + i2)) (pop (pop stack)), state)
  where (IntVal i1) = top stack
        (IntVal i2) = top (pop stack)
run (Mult:code, stack, state) = run (code, push (IntVal (i1 * i2)) (pop (pop stack)), state)
  where (IntVal i1) = top stack
        (IntVal i2) = top (pop stack)
run (Sub:code, stack, state) = run (code, push (IntVal (i1 - i2)) (pop (pop stack)), state)
  where (IntVal i1) = top stack
        (IntVal i2) = top (pop stack)
run (Tru:code, stack, state) = run (code, push (BoolVal True) stack, state)
run (Fals:code, stack, state) = run (code, push (BoolVal False) stack, state)
run (Equ:code, stack, state) = run (code, push (BoolVal (stk1 == stk2)) (pop (pop stack)), state)
  where stk1 = top stack
        stk2 = top (pop stack)
run (Le:code, stack, state) = run (code, push (BoolVal (i1 <= i2)) (pop (pop stack)), state)
  where (IntVal i1) = top stack
        (IntVal i2) = top (pop stack)
run (And:code, stack, state) = run (code, push (BoolVal (b1 && b2)) (pop (pop stack)), state)
  where (BoolVal b1) = top stack
        (BoolVal b2) = top (pop stack)
run (Neg:code, stack, state) = run (code, push (BoolVal (not b)) (pop stack), state)
  where (BoolVal b) = top stack
run ((Fetch str):code, stack, state) = run (code, push (fetch str state) stack, state)
run ((Store str):code, stack, state) = run (code, pop stack, store str (top stack) state)
run (Noop:code, stack, state) = run (code, stack, state)
run ((Branch ifbody elsebody):code, stack, state)
  | b = run (ifbody ++ code, pop stack, state)
  | otherwise = run (elsebody ++ code, pop stack, state)
  where (BoolVal b) = top stack
run ((Loop condition body):code, stack, state) = run (condition ++ [Branch (body ++ [Loop condition body]) [Noop]], stack, state)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- string em codigo -> lista de tokens -> arvore -> lista de instrucoes -> run 
-- Part 2

data Aexp = Num Integer | Var String | AddE Aexp Aexp | SubE Aexp Aexp | MultE Aexp Aexp deriving Show
data Bexp = Bool Bool | EquE Aexp Aexp | LeE Aexp Aexp | BEquE Bexp Bexp | AndE Bexp Bexp | NegE Bexp deriving Show
data Stm = Assign String Aexp | If Bexp [Stm] [Stm] | While Bexp [Stm] deriving Show

type Program = [Stm]

data Token = IntTok Integer | BoolTok Bool | VarTok String | IfTok | ThenTok | ElseTok | WhileTok | DoTok | AssignTok | SemiTok | OpenTok | CloseTok | AddTok | SubTok | MultTok | EquTok | BEquTok | LeTok | AndTok | NegTok
  deriving Show

compA :: Aexp -> Code
compA (Num i) = [Push i]
compA (AddE a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubE a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MultE a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB (Bool b) = if b then [Tru] else [Fals]
compB (EquE a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (LeE a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (AndE b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (NegE b) = compB b ++ [Neg]

compile :: Program -> Code
compile [] = []
compile ((Assign str a):xs) = compA a ++ [Store str] ++ compile xs
compile ((If b s1 s2):xs) = compB b ++ [Branch (compile s1) (compile s2)] ++ compile xs
compile ((While b s):xs) = Loop (compB b ++ [Branch (compile s) [Noop]]) [Noop] : compile xs

{--
type Var = String
data Statement = Print Var | Assign Var Int | Add Var Var | While Var Code  deriving (Eq, Show)

statementParser :: Parser Statement
statementParser = try printParser <|> try assignParser <|> try addParser <|> whileParser
 
printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)
 
assignParser :: Parser Statement
assignParser = Assign <$> (many1 letter) <*> (many1 space >> char '=' >> many1 space >> int)
 
addParser :: Parser Statement
addParser = Add <$> (many1 letter) <*> (many1 space >> string "+=" >> many1 space >> many1 letter)
 
whileParser :: Parser Statement
whileParser = While <$> (string "while" >> many1 space >> many1 letter) <*> (many1 space >> string "positive" >> many1 space >> char '{' >> many1 space >> many (statementParser <* many1 space) <* char '}')
--}

buildData :: [Token] -> Program
buildData [] = []

stmParser :: Parser Stm
stmParser = try assignParser <|> try ifParser -- <|> whileParser

aexpOperators = [ [Infix (MultE <$ char '*') AssocLeft]
                , [Infix (AddE <$ char '+') AssocLeft, Infix (SubE <$ char '-') AssocLeft]
                ]

aexpTerm = parens aexpParser <|> numParser <|> varParser

lexer = T.makeTokenParser haskellDef
parens = T.parens lexer
int = T.integer lexer
identifier = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer

aexpParser :: Parser Aexp
aexpParser = buildExpressionParser aexpOperators aexpTerm

numParser :: Parser Aexp
numParser = Num <$> int

varParser :: Parser Aexp
varParser = Var <$> identifier

bexpOperators = [ [Prefix (NegE <$ reservedOp "not")]
                , [Infix (AndE <$ reservedOp "and") AssocLeft]
                ]

bexpTerm = parens bexpParser <|> try equParser <|> try leParser <|> bequParser <|> try boolParser 

bexpParser :: Parser Bexp
bexpParser = buildExpressionParser bexpOperators bexpTerm

boolParser :: Parser Bexp
boolParser = Bool <$> ((True <$ reserved "True") <|> (False <$ reserved "False"))

equParser :: Parser Bexp
equParser = EquE <$> aexpParser <*> (string "==" *> aexpParser)

leParser :: Parser Bexp
leParser = LeE <$> aexpParser <*> (string "<=" *> aexpParser)

bequParser :: Parser Bexp
bequParser = BEquE <$> bexpParser <*> (string "=" *> bexpParser)

assignParser :: Parser Stm
assignParser = Assign <$> (many1 letter <* spaces <* string ":=" <* spaces) <*> aexpParser <* spaces <* char ';'

ifParser :: Parser Stm
ifParser = If <$> (reserved "if" *> bexpParser) <*> (spaces *> reserved "then" *> spaces *> many (stmParser <* spaces)) <*> (spaces *> reserved "else" *> spaces *> many (stmParser <* spaces)) <* spaces <* char ';'

whileParser :: Parser Stm
whileParser = undefined

parse :: String -> Program
parse input = 
  case P.parse (many (stmParser <* spaces)) "" input of
    Left err -> error (show err)
    Right program -> program

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
