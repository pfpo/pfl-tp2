module Runner (run, stack2Str, state2Str, createEmptyStack, createEmptyState) where

import Compiler

-- Stack data type. Holds a list of StkVal which can be ints or bools
data StkVal = IntVal Integer | BoolVal Bool
data Stack = Stk [StkVal]

instance Eq StkVal where
  (IntVal i1) == (IntVal i2) = i1 == i2
  (BoolVal b1) == (BoolVal b2) = b1 == b2
  _ == _ = error "Run-time error: Expected two values of the same type on the stack"

-- Push a value onto the stack
push :: StkVal -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

-- Pop a value off the stack
pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Run-time error: Expected a non-empty stack"

-- Get the value at the top of the stack
top :: Stack -> StkVal
top (Stk (x:_)) = x
top _ = error "Run-time error: Expected a non-empty stack"

-- Create an empty stack
emptyStack :: Stack
emptyStack = Stk []

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = emptyStack

-- Convert a stack value to a string
stkVal2Str :: StkVal -> String
stkVal2Str (IntVal i) = show i
stkVal2Str (BoolVal b) = show b

-- Convert a stack to a string
stack2Str :: Stack -> String
stack2Str (Stk []) = ""
stack2Str (Stk [x]) = stkVal2Str x
stack2Str (Stk (x:xs)) = stkVal2Str x ++ "," ++ stack2Str (Stk xs)

-- State data type. Holds a list of (String, StkVal) pairs
type State = [(String, StkVal)]

-- Store a value in the state. If the variable already exists, replace it. If it doesn't, add it. Should be ordered alphabetically.
store :: String -> StkVal -> State -> State
store str val [] = [(str, val)]
store str val ((str', val'):xs)
    | str == str' = (str, val):xs
    | str < str' = (str, val):(str', val'):xs
    | otherwise = (str', val'): store str val xs

-- Fetch a value from the state. If the variable doesn't exist, return an error.
fetch :: String -> State -> StkVal
fetch str [] = error "Run-time error: Variable not found in state"
fetch str ((str', val'):xs)
    | str == str' = val'
    | otherwise = fetch str xs

-- Create an empty state
emptyState :: State
emptyState = []

-- Create an empty state
createEmptyState :: State
createEmptyState = emptyState

-- Convert a state to a string
state2Str :: State -> String
state2Str [] = ""
state2Str [(str, val)] = str ++ "=" ++ stkVal2Str val
state2Str ((str, val):xs) = str ++ "=" ++ stkVal2Str val ++ "," ++ state2Str xs

-- Interpreter function. Takes a tuple of (Code, Stack, State), runs the code, and returns a tuple of the final (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push i):code, stack, state) = run (code, push (IntVal i) stack, state)

run (Add:code, stack, state) =
    case (top stack, top (pop stack)) of
        (IntVal i1, IntVal i2) -> run (code, push (IntVal (i1 + i2)) (pop (pop stack)), state)
        _ -> error "Run-time error: Expected IntVal at the top of the stack"

run (Mult:code, stack, state) =
    case (top stack, top (pop stack)) of
        (IntVal i1, IntVal i2) -> run (code, push (IntVal (i1 * i2)) (pop (pop stack)), state)
        _ -> error "Run-time error: Expected IntVal at the top of the stack"

run (Sub:code, stack, state) =
  case (top stack, top (pop stack)) of
    (IntVal i1, IntVal i2) -> run (code, push (IntVal (i1 - i2)) (pop (pop stack)), state)
    _ -> error "Run-time error: Expected IntVal at the top of the stack"

run (Tru:code, stack, state) = run (code, push (BoolVal True) stack, state)
run (Fals:code, stack, state) = run (code, push (BoolVal False) stack, state)
run (Equ:code, stack, state) = run (code, push (BoolVal (stk1 == stk2)) (pop (pop stack)), state)
    where stk1 = top stack
          stk2 = top (pop stack)

run (Le:code, stack, state) =
    case (top stack, top (pop stack)) of
        (IntVal i1, IntVal i2) -> run (code, push (BoolVal (i1 <= i2)) (pop (pop stack)), state)
        _ -> error "Run-time error: Expected IntVal at the top of the stack"

run (And:code, stack, state) = 
    case (top stack, top (pop stack)) of
        (BoolVal b1, BoolVal b2) -> run (code, push (BoolVal (b1 && b2)) (pop (pop stack)), state)
        _ -> error "Run-time error: Expected BoolVal at the top of the stack"

run (Neg:code, stack, state) =
    case top stack of
        (BoolVal b) -> run (code, push (BoolVal (not b)) (pop stack), state)
        _ -> error "Run-time error: Expected BoolVal at the top of the stack"

run ((Fetch str):code, stack, state) = run (code, push (fetch str state) stack, state)
run ((Store str):code, stack, state) = run (code, pop stack, store str (top stack) state)
run (Noop:code, stack, state) = run (code, stack, state)
run ((Branch ifbody elsebody):code, stack, state) =
    case top stack of
        BoolVal b -> if b then run (ifbody ++ code, pop stack, state)
                          else run (elsebody ++ code, pop stack, state)
        _ -> error "Run-time error: Expected BoolVal at the top of the stack"

run ((Loop condition body):code, stack, state) = run (condition ++ [Branch (body ++ [Loop condition body]) [Noop]] ++ code, stack, state)
