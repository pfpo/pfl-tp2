module Compiler (compile, Inst(..), Code) where

import Parser

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Compiles arithmetic expressions into code
compA :: Aexp -> Code
compA (IntVal i) = [Push i]
compA (Var str) = [Fetch str]
compA (ABinary AddOp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (ABinary SubOp a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (ABinary Mul a1 a2) = compA a1 ++ compA a2 ++ [Mult]

-- Compiles boolean expressions into code
compB :: Bexp -> Code
compB (BoolVal b) = if b then [Tru] else [Fals]
compB (RBinary Equal a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (RBinary LessEq a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (BBinary AndOp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (BBinary BEqual b1 b2) = compB b1 ++ compB b2 ++ [Equ]
compB (Not b) = compB b ++ [Neg]

-- Compiles statements into code. Statements are: 
-- Seq (list of statements) 
-- Assign (variable name, arithmetic expression)
-- If (boolean expression, statement, statement)
-- While (boolean expression, statement)
compile :: Program -> Code
compile (Seq []) = []
compile (Seq (x:xs)) = compile x ++ compile (Seq xs)
compile (Assign var aexp) = compA aexp ++ [Store var]
compile (If bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
compile (While bexp stm) = [Loop (compB bexp) (compile stm)]
